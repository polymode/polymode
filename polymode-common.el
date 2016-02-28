;;  -*- lexical-binding: t -*-
;; COMMON INITIALIZATION, UTILITIES and INTERNALS which didn't fit anywhere else

(eval-when-compile
  (require 'cl))
(require 'font-lock)
(require 'color)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)
(require 'format-spec)

(defcustom polymode-display-process-buffers t
  "When non-nil, display weaving and exporting process buffers."
  :group 'polymode
  :type 'boolean)

(defvar polymode-switch-buffer-hook nil
  "Hook run on switching to a different buffer.
Each function is run with two arguments `old-buffer' and
`new-buffer'. This hook is commonly used to transfer state
between buffers. The hook is run in a new buffer, but you should
not rely on that.")

;; esential vars
(defvar-local pm/polymode nil)
(defvar-local pm/chunkmode nil)
(defvar-local pm/type nil)
(defvar-local pm--fontify-region-original nil)
(defvar-local pm--indent-line-function-original nil)
(defvar-local pm--syntax-begin-function-original nil)
;; (defvar-local pm--killed-once nil)
(defvar-local polymode-mode nil
  "This variable is t if current \"mode\" is a polymode.")

;; silence the compiler for now
(defvar pm--output-file nil)
(defvar pm--input-buffer nil)
(defvar pm--input-file nil)
(defvar pm--export-spec nil)
(defvar pm/type)
(defvar pm/polymode)
(defvar pm/chunkmode)
(defvar *span*)

;; core api from polymode.el, which relies on polymode-methods.el.
;; fixme: some of these are not api, rename
(declare-function pm-base-buffer "polymode")
(declare-function pm-get-innermost-span "polymode")
(declare-function pm-map-over-spans "polymode")
(declare-function pm-narrow-to-span "polymode")
(declare-function poly-lock-fontify-region "poly-lock")
(declare-function pm-syntax-begin-function "polymode")

;; methods api from polymode-methods.el
(declare-function pm-initialize "polymode-methods")
(declare-function pm-get-buffer "polymode-methods")
(declare-function pm-select-buffer "polymode-methods")
(declare-function pm-install-buffer "polymode-methods")
(declare-function pm-get-adjust-face "polymode-methods")
(declare-function pm-get-span "polymode-methods")
(declare-function pm-indent-line "polymode-methods")

;; other locals
(defvar-local pm--process-buffer nil)


;;; UTILITIES
(defun pm--display-file (ofile)
  (display-buffer (find-file-noselect ofile 'nowarn)))

(defun pm--get-mode-symbol-from-name (str &optional no-fallback)
  "Guess and return mode function."
  (let* ((str (if (symbolp str)
                  (symbol-name str)
                str))
         (mname (if (string-match-p "-mode$" str)
                    str
                  (concat str "-mode"))))
    (pm--get-existent-mode (intern mname) no-fallback)))

(defun pm--get-existent-mode (mode &optional no-fallback)
  "Check if MODE symbol is defined and is a valid function.
If so, return it, otherwise return 'fundamental-mode and issue a
warning."
  (cond ((fboundp mode) mode)
        (no-fallback nil)
        (t (message "Cannot find function `%s', using `fundamental-mode'" mode)
           'fundamental-mode)))

(defun pm--oref-with-parents (object slot)
  "Merge slots SLOT from the OBJECT and all its parent instances."
  (let (VALS)
    (while object
      (setq VALS (append (and (slot-boundp object slot) ; don't cascade
                              (eieio-oref object slot))
                         VALS)
            object (and (slot-boundp object :parent-instance)
                        (oref object :parent-instance))))
    VALS))

(defun pm--abrev-names (list abrev-regexp)
  "Abreviate names in LIST by replacing abrev-regexp with empty
string."
  (mapcar (lambda (nm)
            (let ((str-nm (if (symbolp nm)
                              (symbol-name nm)
                            nm)))
              (cons (replace-regexp-in-string abrev-regexp "" str-nm)
                    str-nm)))
          list))

(defun pm--put-hist (key val)
  (oset pm/polymode -hist
        (plist-put (oref pm/polymode -hist) key val)))

(defun pm--get-hist (key)
  (plist-get (oref pm/polymode -hist) key))

(defun pm--comment-region (beg end)
  ;; mark as syntactic comment
  (when (> end 1)
    (with-silent-modifications
      (let ((beg (or beg (region-beginning)))
            (end (or end (region-end))))
        (let ((ch-beg (char-after beg))
              (ch-end (char-before end)))
          (add-text-properties beg (1+ beg)
                               (list 'syntax-table (cons 11 ch-beg)
                                     'rear-nonsticky t
                                     'polymode-comment 'start))
          (add-text-properties (1- end) end
                               (list 'syntax-table (cons 12 ch-end)
                                     'rear-nonsticky t
                                     'polymode-comment 'end)))))))

(defun pm--uncomment-region (beg end)
  ;; remove all syntax-table properties. Should not cause any problem as it is
  ;; always used before font locking
  (when (> end 1)
    (with-silent-modifications
      (let ((props '(syntax-table nil rear-nonsticky nil polymode-comment nil)))
        (remove-text-properties beg end props)
        ;; (remove-text-properties beg (1+ beg) props)
        ;; (remove-text-properties end (1- end) props)
        ))))

(defun pm--wrap-callback (processor slot ifile)
  ;; replace processor :sentinel or :callback temporally in order to export-spec as a
  ;; followup step or display the result
  (let ((sentinel1 (eieio-oref processor slot))
        (cur-dir default-directory)
        (exporter (symbol-value (oref pm/polymode :exporter)))
        (obuffer (current-buffer)))
    (if pm--export-spec
        (let ((espec pm--export-spec))
          (lambda (&rest args)
            (with-current-buffer obuffer
              (let ((wfile (apply sentinel1 args))
                    (pm--export-spec nil))
                (if wfile
                    (pm-export exporter (car espec) (cdr espec) wfile)
                  (error "Callback didn't return processed file."))))))
      (lambda (&rest args)
        (with-current-buffer obuffer
          (let ((wfile (apply sentinel1 args)))
            (if wfile
                (pm--display-file (expand-file-name wfile cur-dir))
              (error "Callback didn't return processed file."))))))))

(defun pm--run-shell-command (command sentinel buff-name message)
  "Run shell command interactively.
Run command in a buffer (in comint-shell-mode) in order to be
able to accept user interaction."
  ;; simplified version of TeX-run-TeX
  (require 'comint)
  (let* ((buffer (get-buffer-create buff-name))
         (process nil)
         (command-buff (current-buffer))
         (ofile pm--output-file)
         ;; (command (shell-quote-argument command))
         )
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert message)
      (comint-exec buffer buff-name shell-file-name nil
                   (list shell-command-switch command))
      (setq process (get-buffer-process buffer))
      (comint-mode)
      (set-process-sentinel process sentinel)
      (setq pm--process-buffer t)
      (set-marker (process-mark process) (point-max))
      ;; for communication with sentinel
      (process-put process :output-file pm--output-file)
      (process-put process :input-file pm--input-file))
    (when polymode-display-process-buffers
      (display-buffer buffer `(nil . ((inhibit-same-window . ,pop-up-windows)))))
    nil))

(defun pm--make-shell-command-sentinel (error-regexp action)
  (lambda (process name)
    "Sentinel built with `pm--make-shell-command-sentinel'."
    (let ((buff (process-buffer process)))
      (with-current-buffer buff
        ;; fixme: remove this later
        (sit-for .2)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (if (not (re-search-forward error-regexp nil 'no-error))
              (progn
                (display-buffer (current-buffer))
                (message "Done with %s" action)
                (process-get process :output-file))              
            (error "Bumps while %s (%s)" action name)))))))

(provide 'polymode-common)

