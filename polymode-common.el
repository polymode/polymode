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

(defcustom polymode-skip-processing-when-unmodified t
  "If non-nil, consider modification times of input and output files.
Skip weaving or exporting process when output file is more recent
than the input file."
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
;; (defvar-local pm--killed-once nil)
(defvar-local polymode-mode nil
  "This variable is t if current \"mode\" is a polymode.")

;; silence the compiler for now
(defvar pm--output-file nil)
(defvar pm--input-buffer nil)
(defvar pm--input-file nil)
(defvar pm--export-spec nil)
(defvar pm--input-not-real nil)
(defvar pm--output-not-real nil)
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
  (when ofile
   ;; errors might occur (most notably with open-with package errors are intentional)
   ;; We need to catch those if we want to display multiple files like with Rmarkdown
    (condition-case err
        (let ((buff (get-file-buffer ofile)))
          ;; silently kill and re-open
          (when buff
            (with-current-buffer buff
                (revert-buffer t t)))
          (display-buffer (find-file-noselect ofile 'nowarn)))
      (error (message "Error while displaying '%s': %s"
                      (file-name-nondirectory ofile)
                      (error-message-string err))))))

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
If so, return it, otherwise return `poly-fallback-mode' and issue
a warning."
  (cond ((fboundp mode) mode)
        (no-fallback nil)
        (t (message "Cannot find function `%s', using `poly-fallback-mode'" mode)
           'poly-fallback-mode)))

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
  "Abbreviate names in LIST by replacing abrev-regexp with empty string."
  (mapcar (lambda (nm)
            (let ((str-nm (if (symbolp nm)
                              (symbol-name nm)
                            nm)))
              (cons (replace-regexp-in-string abrev-regexp "" str-nm)
                    str-nm)))
          list))

(defun pm--prop-put (key val &optional object)
  (oset (or object pm/polymode) -props
        (plist-put (oref (or object pm/polymode) -props) key val)))

(defun pm--prop-get (key &optional object)
  (plist-get (oref (or object pm/polymode) -props) key))

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

(defun pm--completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Wrapper for `completing-read'.
Takes care when collection is an alist of (name . meta-info). If
so, asks for names, but returns meta-info for that name. Enforce
require-match = t. Also takes care of adding the most relevant
DEF from history."
  (if (and (listp collection)
           (listp (car collection)))
      (let* ((candidates (mapcar #'car collection))
             (thist (and hist
                         (delq nil (mapcar (lambda (x) (car (member x candidates)))
                                           (symbol-value hist)))))
             (def (or def (car thist))))
        (assoc (completing-read prompt candidates predicate t initial-input hist def inherit-input-method)
               collection))
    (completing-read prompt candidates predicate require-match initial-input hist def inherit-input-method)))


;; Weaving and Exporting common utilities

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
                    (pm--export-spec nil)
                    (pm--input-not-real t))
                ;; If no wfile, probably errors occurred. So we stop.
                (when wfile
                  (when (listp wfile)
                    ;; In an unlikely situation weaver can generate multiple
                    ;; files. Pick the first one.
                    (setq wfile (car wfile)))
                  (pm-export exporter (car espec) (cdr espec) wfile))))))
      (lambda (&rest args)
        (with-current-buffer obuffer
          (let ((ofile (apply sentinel1 args)))
            (when ofile
              (let ((ofiles (if (listp ofile) ofile (list ofile))))
                (dolist (f ofiles)
                  (pm--display-file (expand-file-name f cur-dir)))))))))))

(defun pm--file-mod-time (file)
  (and (stringp file)
       (file-exists-p file)
       (nth 5 (file-attributes file))))

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
         ;; weave/export buffers are re-usable; need to transfer some vars
         (dd default-directory)
         ;; (command (shell-quote-argument command))
         )
    (with-current-buffer buffer
      (setq-local default-directory dd)
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
      (process-put process :output-file-mod-time (pm--file-mod-time pm--output-file))
      (process-put process :input-file pm--input-file)
      (when polymode-display-process-buffers
        (display-buffer buffer `(nil . ((inhibit-same-window . ,pop-up-windows)))))
      nil)))

(defun pm--make-shell-command-sentinel (action)
  (lambda (process name)
    "Sentinel built with `pm--make-shell-command-sentinel'."
    (let ((buff (process-buffer process))
          (status (process-exit-status process)))
      (if (> status 0)
          (progn
            (message "Errors during %s; process exit status %d" action status)
            (ding) (sit-for 1)
            nil)
        (with-current-buffer buff
          (let ((ofile (process-get process :output-file)))
            (cond
             ;; 1. output-file guesser
             ((functionp ofile) (funcall ofile))
             ;; 2. string
             (ofile
              (let ((otime (process-get process :output-file-mod-time))
                    (ntime (pm--file-mod-time ofile)))
                (if (or (null ntime)
                        (and otime
                             (not (time-less-p otime ntime))))
                    ;; mod time didn't change
                    ;; tothink: shall we still return ofile for display?
                    (progn
                      (display-buffer (current-buffer))
                      (message "Output file unchanged. Either input unchanged or errors during %s." action)
                      (ding) (sit-for 1)
                      ofile)
                  ;; else, all is good, we return the file name
                  ;; (display-buffer (current-buffer))
                  (message "Done with %s" action)
                  ofile)))
             ;; 3. output file is not known; display process buffer
             (t (display-buffer (current-buffer)) nil))))))))

(fset 'pm-default-export-sentinel (pm--make-shell-command-sentinel "export"))
(fset 'pm-default-shell-weave-sentinel (pm--make-shell-command-sentinel "weaving"))

(defun pm--make-selector (specs elements)
  (cond ((listp elements)
         (let ((spec-alist (cl-mapcar #'cons specs elements)))
           (lambda (selsym &rest ignore)
             (cdr (assoc selsym spec-alist)))))
        ((functionp elements) elements)
        (t (error "elements argument must be either a list or a function"))))

(defun pm--selector (processor type id)
  (let ((spec (or (assoc id (eieio-oref processor type))
                  (error "%s spec '%s' cannot be found in '%s'"
                         (symbol-name type) id (eieio-object-name processor))))
        (names (cond
                ;; exporter slots
                ((eq type :from) '(regexp doc command))
                ((eq type :to) '(ext doc t-spec))
                ;; weaver slot
                ((eq type :from-to) '(regexp ext doc command))
                (t (error "invalid type '%s'" type)))))
    (pm--make-selector names (cdr spec))))

(defun pm--selector-match (selector &optional file)
  (or (funcall selector 'match file)
      (string-match-p (funcall selector 'regexp)
                      (or file buffer-file-name))))

(defun pm--selectors (processor type)
  (let ((ids (mapcar #'car (eieio-oref processor type))))
    (mapcar (lambda (id) (cons id (pm--selector processor type id))) ids)))

(defun pm--output-command.file (output-file-format sfrom &optional sto quote)
  ;; !!Must be run in input buffer!!
  (cl-flet ((squote (arg) (or (and (stringp arg)
                                   (if quote (shell-quote-argument arg) arg))
                              "")))
    (let* ((base-ofile (or (funcall (or sto sfrom) 'output-file)
                           (let ((ext (funcall (or sto sfrom) 'ext)))
                             (when ext
                               (concat (format output-file-format
                                               (file-name-base buffer-file-name))
                                       "." ext)))))
           (ofile (and (stringp base-ofile)
                       (expand-file-name base-ofile)))
           (oname (and (stringp base-ofile)
                       (file-name-base base-ofile)))
           (t-spec (and sto (funcall sto 't-spec)))
           (command-w-formats (or (and sto (funcall sto 'command))
                                  (and (listp t-spec) (car t-spec))
                                  (funcall sfrom 'command)))
           (command (format-spec command-w-formats
                                 (list (cons ?i (squote (file-name-nondirectory buffer-file-name)))
                                       (cons ?I (squote buffer-file-name))
                                       (cons ?o (squote base-ofile))
                                       (cons ?O (squote ofile))
                                       (cons ?b (squote oname))
                                       (cons ?t (squote t-spec))))))
      (cons command (or ofile base-ofile)))))

(defun pm--process-internal (processor from to ifile &optional callback shell-quote)
  (let ((is-exporter (object-of-class-p processor 'pm-exporter)))
    (if is-exporter
        (unless (and from to)
          (error "For exporter both FROM and TO must be supplied (from: %s, to: %s)" from to))
      (unless from
        ;; it represents :from-to slot
        (error "For weaver FROM must be supplied (from: %s)" from)))
    (let* ((sfrom (if is-exporter
                      (pm--selector processor :from from)
                    (pm--selector processor :from-to from)))
           (sto (and is-exporter (pm--selector processor :to to)))
           (ifile (or ifile buffer-file-name))
           ;; fixme: nowarn is only right for inputs from weavers, you need to
           ;; save otherwise
           (ibuffer (if pm--input-not-real
                        ;; for exporter input we silently re-fetch the file
                        ;; even if it was modified
                        (find-file-noselect ifile t)
                      ;; if real user file, get it or fetch it
                      (or (get-file-buffer ifile)
                          (find-file-noselect ifile))))
           (output-file-format (if is-exporter
                                   polymode-exporter-output-file-format
                                 polymode-weave-output-file-format)))
      (with-current-buffer ibuffer
        (save-buffer)
        (let ((comm.ofile (pm--output-command.file output-file-format sfrom sto)))
          (message "%s '%s' with '%s' ..." (if is-exporter "Exporting" "Weaving")
                   (file-name-nondirectory ifile) (eieio-object-name processor))
          (let* ((pm--output-file (cdr comm.ofile))
                 (pm--input-file ifile)
                 ;; skip weaving step if possible
                 ;; :fixme this should not happen after weaver/exporter change
                 ;; or after errors in previous exporter
                 (omt (and polymode-skip-processing-when-unmodified
                           (stringp pm--output-file)
                           (pm--file-mod-time pm--output-file)))
                 (imt (and omt (pm--file-mod-time pm--input-file)))
                 (ofile (or (and imt (time-less-p imt omt) pm--output-file)
                            (let ((fun (oref processor :function))
                                  (args (delq nil (list callback from to))))
                              (apply fun (car comm.ofile) args)))))
            ;; ofile is non-nil in two cases:
            ;;  -- synchronous back-ends (very uncommon)
            ;;  -- when output is transitional (not real) and mod time of input < output  
            (when ofile
              (if pm--export-spec
                  ;; same logic as in pm--wrap-callback
                  (let ((pm--input-not-real t)
                        (espec pm--export-spec)
                        (pm--export-spec nil))
                    (when (listp ofile)
                      (setq ofile (car ofile)))
                    (pm-export (symbol-value (oref pm/polymode :exporter))
                               (car espec) (cdr espec)
                               ofile))
                (pm--display-file ofile)))))))))

(provide 'polymode-common)

