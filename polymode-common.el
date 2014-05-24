;; COMMON INITIALIZATION, UTILITIES and INTERNALS which didn't fit anywhere else

(eval-when-compile
  (require 'cl))
(require 'font-lock)
(require 'color)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)

;; esential vars
(defvar-local pm/polymode nil)
(defvar-local pm/chunkmode nil)
(defvar-local pm/type nil)
(defvar-local polymode-major-mode nil)
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
(defvar pm/type)
(defvar pm/polymode)
(defvar pm/chunkmode)
(defvar *span*)

;; core api from polymode.el, which relies on polymode-methods.el.
;; fixme: some of these are not api, rename
(declare-function pm/base-buffer "polymode")
(declare-function pm/get-innermost-span "polymode")
(declare-function pm/map-over-spans "polymode")
(declare-function pm/narrow-to-span "polymode")
(declare-function pm/fontify-region "polymode")
(declare-function pm/syntax-begin-function "polymode")

;; methods api from polymode-methods.el
(declare-function pm-initialize "polymode-methods")
(declare-function pm-get-buffer "polymode-methods")
(declare-function pm-select-buffer "polymode-methods")
(declare-function pm-install-buffer "polymode-methods")
(declare-function pm-get-adjust-face "polymode-methods")
(declare-function pm-get-span "polymode-methods")
(declare-function pm-indent-line "polymode-methods")

;; buffer manipulation function in polymode-methods.el
;; polymode-common.el:315:1:Warning: the following functions are not known to be defined:
;; pm--create-indirect-buffer, pm--setup-buffer, pm--span-at-point, polymode-select-buffer

;; temporary debugging facilities
(defvar pm--dbg-mode-line t)
(defvar pm--dbg-fontlock t)
(defvar pm--dbg-hook t)

;; other locals
(defvar-local pm--process-buffer nil)


;;; UTILITIES
(defun pm--display-file (ofile)
  (display-buffer (find-file-noselect ofile 'nowarn)))

(defun pm--get-indirect-buffer-of-mode (mode)
  (loop for bf in (oref pm/polymode -buffers)
        when (and (buffer-live-p bf)
                  (eq mode (buffer-local-value 'polymode-major-mode bf)))
        return bf))

;; ;; This doesn't work in 24.2, pcase bug ((void-variable xcar))
;; ;; Other pcases in this file don't throw this error
;; (defun pm--set-chunkmode-buffer (obj type buff)
;;   (with-slots (buffer head-mode head-buffer tail-mode tail-buffer) obj
;;     (pcase (list type head-mode tail-mode)
;;       (`(body body ,(or `nil `body))
;;        (setq buffer buff
;;              head-buffer buff
;;              tail-buffer buff))
;;       (`(body ,_ body)
;;        (setq buffer buff
;;              tail-buffer buff))
;;       (`(body ,_ ,_ )
;;        (setq buffer buff))
;;       (`(head ,_ ,(or `nil `head))
;;        (setq head-buffer buff
;;              tail-buffer buff))
;;       (`(head ,_ ,_)
;;        (setq head-buffer buff))
;;       (`(tail ,_ ,(or `nil `head))
;;        (setq tail-buffer buff
;;              head-buffer buff))
;;       (`(tail ,_ ,_)
;;        (setq tail-buffer buff))
;;       (_ (error "type must be one of 'body 'head and 'tail")))))

;; a literal transcript of the pcase above
(defun pm--set-chunkmode-buffer (obj type buff)
  (with-slots (-buffer head-mode -head-buffer tail-mode -tail-buffer) obj
    (cond
     ((and (eq type 'body)
           (eq head-mode 'body)
           (or (null tail-mode)
               (eq tail-mode 'body)))
      (setq -buffer buff
            -head-buffer buff
            -tail-buffer buff))
     ((and (eq type 'body)
           (eq tail-mode 'body))
      (setq -buffer buff
            -tail-buffer buff))
     ((eq type 'body)
      (setq -buffer buff))
     ((and (eq type 'head)
           (or (null tail-mode)
               (eq tail-mode 'head)))
      (setq -head-buffer buff
            -tail-buffer buff))
     ((eq type 'head)
      (setq -head-buffer buff))
     ((and (eq type 'tail)
           (or (null tail-mode)
               (eq tail-mode 'head)))
      (setq -tail-buffer buff
            -head-buffer buff))
     ((eq type 'tail)
      (setq -tail-buffer buff))
     (t (error "type must be one of 'body 'head and 'tail")))))

(defun pm--get-chunkmode-mode (obj type)
  (with-slots (mode head-mode tail-mode) obj
    (cond ((or (eq type 'body)
               (and (eq type 'head)
                    (eq head-mode 'body))
               (and (eq type 'tail)
                    (or (eq tail-mode 'body)
                        (and (null tail-mode)
                             (eq head-mode 'body)))))
           (oref obj :mode))
          ((or (and (eq type 'head)
                    (eq head-mode 'host))
               (and (eq type 'tail)
                    (or (eq tail-mode 'host)
                        (and (null tail-mode)
                             (eq head-mode 'host)))))
           (oref (oref pm/polymode -hostmode) :mode))
          ((eq type 'head)
           (oref obj :head-mode))
          ((eq type 'tail)
           (oref obj :tail-mode))
          (t (error "type must be one of 'head 'tail 'body")))))

(defun pm--create-chunkmode-buffer-maybe (chunkmode type)
  ;; assumes pm/polymode is set
  (let ((mode (pm--get-chunkmode-mode chunkmode type)))
    (or (pm--get-indirect-buffer-of-mode mode)
        (let ((buff (pm--create-indirect-buffer mode)))
          (with-current-buffer  buff
            (setq pm/chunkmode chunkmode)
            (setq pm/type type)
            (pm--setup-buffer)
            (funcall (oref pm/polymode :minor-mode))
            buff)))))

(defun pm--get-mode-symbol-from-name (str)
  "Guess and return mode function.
Return major mode function constructed from STR by appending
'-mode' if needed. If the constructed symbol is not a function
return an error."
  (let ((mname (if (string-match-p "-mode$" str)
                   str
                 (concat str "-mode"))))
    (pm--get-available-mode (intern mname))))

(defun pm--get-available-mode (mode)
  "Check if MODE symbol is defined and is a valid function.
If so, return it, otherwise return 'fundamental-mode with a
warnign."
  (cond ((fboundp mode) mode)
        (t (message "Cannot find %s function, using 'fundamental-mode instead" mode)
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
              (propertize (replace-regexp-in-string abrev-regexp "" str-nm)
                          :orig str-nm)))
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

(defun pm--run-command (command sentinel buff-name message)
  "Run command interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts
user interaction."
  ;; simplified version of TeX-run-TeX
  (require 'comint)
  (let* ((buffer (get-buffer-create buff-name))
         (process nil)
         (command-buff (current-buffer))
         (ofile pm--output-file))
    (with-current-buffer buffer
      (setq pm--process-buffer t)
      (read-only-mode -1)
      (erase-buffer)
      (insert message)
      (comint-exec buffer buff-name shell-file-name nil
                   (list shell-command-switch command))
      (comint-mode)
      (setq process (get-buffer-process buffer))
      (set-process-sentinel process sentinel)
      ;; communicate with sentinel
      (set (make-local-variable 'pm--output-file) ofile)
      (set (make-local-variable 'pm--input-buffer) command-buff)
      (set-marker (process-mark process) (point-max)))
    nil))

(defun pm--run-command-sentinel (process name message)
  (let ((buff (process-buffer process)))
    (with-current-buffer buff
      ;; fixme: remove this later
      (sit-for 1)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (if (not (re-search-forward "error" nil 'no-error))
            pm--output-file
          (display-buffer (current-buffer))
          (error "Bumps while %s (%s)" message name))))))


;;; COMPATIBILITY and FIXES
(defun pm--flyspel-dont-highlight-in-chunkmodes (beg end poss)
  (or (get-text-property beg 'chunkmode)
      (get-text-property beg 'chunkmode)))

(defvar object-name)
(defun pm--object-name (object)
  (if (fboundp 'eieio--object-name)
      (eieio--object-name object)
    (aref object object-name)))

(defun pm--activate-jit-lock-mode-maybe ()
  ;; ugly hack for emacs 24.4
  (when (and (string< "24.4" emacs-version)
             ;; jit-lock is deactivated after this one is set.
             jit-lock-functions)

    (setq jit-lock-mode t)
    ;; Mark the buffer for refontification.
    (jit-lock-refontify)

    ;; Install an idle timer for stealth fontification.
    (when (and jit-lock-stealth-time (null jit-lock-stealth-timer))
      (setq jit-lock-stealth-timer
            (run-with-idle-timer jit-lock-stealth-time t
                                 'jit-lock-stealth-fontify)))

    ;; Create, but do not activate, the idle timer for repeated
    ;; stealth fontification.
    (when (and jit-lock-stealth-time (null jit-lock-stealth-repeat-timer))
      (setq jit-lock-stealth-repeat-timer (timer-create))
      (timer-set-function jit-lock-stealth-repeat-timer
                          'jit-lock-stealth-fontify '(t)))

    ;; Init deferred fontification timer.
    (when (and jit-lock-defer-time (null jit-lock-defer-timer))
      (setq jit-lock-defer-timer
            (run-with-idle-timer jit-lock-defer-time t
                                 'jit-lock-deferred-fontify)))

    ;; Initialize contextual fontification if requested.
    (when (eq jit-lock-contextually t)
      (unless jit-lock-context-timer
        (setq jit-lock-context-timer
              (run-with-idle-timer jit-lock-context-time t
                                   'jit-lock-context-fontify)))
      (setq jit-lock-context-unfontify-pos
            (or jit-lock-context-unfontify-pos (point-max))))

    ;; Setup our hooks.
    (add-hook 'after-change-functions 'jit-lock-after-change nil t)
    (add-hook 'fontification-functions 'jit-lock-function)))


;;; DEBUG STUFF
(defun pm--map-over-spans-highlight ()
  (interactive)
  (pm/map-over-spans (lambda ()
                       (let ((start (nth 1 *span*))
                             (end (nth 2 *span*)))
                         (ess-blink-region start end)
                         (sit-for 1)))
                     (point-min) (point-max)))

(defun pm--highlight-span (&optional hd-matcher tl-matcher)
  (interactive)
  (let* ((hd-matcher (or hd-matcher (oref pm/chunkmode :head-reg)))
         (tl-matcher (or tl-matcher (oref pm/chunkmode :tail-reg)))
         (span (pm--span-at-point hd-matcher tl-matcher)))
    (ess-blink-region (nth 1 span) (nth 2 span))
    (message "%s" span)))

(defun pm--run-over-check ()
  (interactive)
  (goto-char (point-min))
  (let ((start (current-time))
        (count 1))
    (polymode-select-buffer)
    (while (< (point) (point-max))
      (setq count (1+ count))
      (forward-char)
      (polymode-select-buffer))
    (let ((elapsed  (float-time (time-subtract (current-time) start))))
      (message "elapsed: %s  per-char: %s" elapsed (/ elapsed count)))))

(provide 'polymode-common)

