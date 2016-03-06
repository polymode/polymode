;;; polymode.el ---  Various tools for debugging and tracing polymode

(defvar pm--underline-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  '(:underline (:color "red" :style wave)))
    overlay)
  "Overlay used in `pm-debug-mode'.")

(defvar pm--inverse-video-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  '(:inverse-video t))
    overlay)
  "Overlay used by `pm-debug-map-over-spans-and-highlight'.")

(defvar pm-debug-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n M-i") 'pm-debug-info-on-current-span)
    (define-key map (kbd "M-n i") 'pm-debug-info-on-current-span)
    (define-key map (kbd "M-n M-p") 'pm-debug-print-relevant-variables)

    (define-key map (kbd "M-n M-t m") 'pm-debug-toogle-info-message)
    (define-key map (kbd "M-n M-t f") 'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-t M-f") 'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-t p") 'pm-debug-toggle-post-command)
    (define-key map (kbd "M-n M-t c") 'pm-debug-toggle-after-change)
    (define-key map (kbd "M-n M-t M-c") 'pm-debug-toggle-after-change)
    (define-key map (kbd "M-n M-t a") 'pm-debug-toggle-all)
    (define-key map (kbd "M-n M-t M-a") 'pm-debug-toggle-all)
    (define-key map (kbd "M-n M-t t") 'pm-debug-trace-relevant-functions)
    (define-key map (kbd "M-n M-t M-t") 'pm-debug-trace-relevant-functions)
    (define-key map (kbd "M-n M-t u") 'pm-debug-untrace-relevant-functions)
    (define-key map (kbd "M-n M-t M-u") 'pm-debug-untrace-relevant-functions)
    (define-key map (kbd "M-n M-h") 'pm-debug-map-over-spans-and-highlight)

    (define-key map (kbd "M-n M-f t") 'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-f s") 'pm-debug-fontify-current-span)
    (define-key map (kbd "M-n M-f e") 'pm-debug-fontify-last-font-lock-error)
    (define-key map (kbd "M-n M-f h") 'pm-debug-highlight-last-font-lock-error-region)
    (define-key map (kbd "M-n M-f M-t") 'pm-debug-toggle-fontification)
    (define-key map (kbd "M-n M-f M-s") 'pm-debug-fontify-current-span)
    (define-key map (kbd "M-n M-f M-e") 'pm-debug-fontify-last-font-lock-error)
    map))

(define-minor-mode pm-debug-minor-mode
  "Turns on/off useful facilities for debugging polymode.

Key bindings:
\\{pm-debug-minor-mode-map}"
  nil
  " PMDBG"
  :group 'polymode
  (interactive)
  (if pm-debug-minor-mode
      (progn
        ;; this is global hook. No need to complicate with local hooks
        (add-hook 'post-command-hook 'pm-debug-highlight-current-span))
    (delete-overlay pm--underline-overlay)
    (delete-overlay pm--inverse-video-overlay)
    (remove-hook 'post-command-hook 'pm-debug-highlight-current-span)))

(defun pm-debug-minor-mode-on ()
  ;; activating everywhere (in case font-lock infloops in a polymode buffer )
  ;; this doesn't activate in fundamental mode
  (pm-debug-minor-mode t))

(define-globalized-minor-mode pm-debug-mode pm-debug-minor-mode pm-debug-minor-mode-on)

(defun pm-debug-highlight-current-span ()
  (when polymode-mode
    (unless (memq this-command '(pm-debug-info-on-current-span
                                 pm-debug-highlight-last-font-lock-error-region))
      (delete-overlay pm--inverse-video-overlay))
    (condition-case err
        (let ((span (pm-get-innermost-span)))
          (when pm-debug-display-info-message
            (pm--debug-info span))
          (move-overlay pm--underline-overlay (nth 1 span) (nth 2 span) (current-buffer)))
      (error (message "%s" (error-message-string err))))))

(defgeneric pm-debug-info (chunkmode))
(defmethod pm-debug-info (chunkmode)
  (format "class:%s" (eieio-object-class-name chunkmode)))
(defmethod pm-debug-info ((chunkmode pm-hbtchunkmode))
  (format "head-reg:\"%s\" tail-reg:\"%s\" %s"
          (oref chunkmode :head-reg) (oref chunkmode :tail-reg)
          (call-next-method)))
(defmethod pm-debug-info ((chunkmode pm-hbtchunkmode))
  (format "head-reg:\"%s\" tail-reg:\"%s\" %s"
          (oref chunkmode :head-reg) (oref chunkmode :tail-reg)
          (call-next-method)))
(defmethod pm-debug-info ((chunkmode pm-hbtchunkmode-auto))
          (call-next-method))

(defun pm--debug-info (&optional span)
  (let* ((span (or span (and polymode-mode (pm-get-innermost-span))))
         (message-log-max nil)
         (beg (nth 1 span))
         (end (nth 2 span))
         (obj (nth 3 span))
         (type (and span (or (car span) 'host))))
    (list (current-buffer)
          (point-min) (point) (point-max)
          major-mode
          type beg end
          (and obj (pm-debug-info obj))
          (format "lppss:%s"
                  syntax-ppss-last))))

(defun pm-debug-info-on-current-span ()
  (interactive)
  (if (not polymode-mode)
      (message "not in a polymode buffer")
    (let ((span (pm-get-innermost-span)))
      (apply 'message "min:%d pos:%d max:%d || (%s) type:%s span:%s-%s %s" (pm--debug-info span))
      (move-overlay pm--inverse-video-overlay (nth 1 span) (nth 2 span) (current-buffer)))))

(defvar pm-debug-display-info-message nil)
(defun pm-debug-toogle-info-message ()
  (interactive)
  (setq pm-debug-display-info-message (not pm-debug-display-info-message)))

(defun pm-debug-toggle-fontification ()
  (interactive)
  (if pm-allow-fontification
      (progn
        (message "fontificaiton disabled")
        (setq pm-allow-fontification nil))
    (message "fontificaiton enabled")
    (setq pm-allow-fontification t)))

(defun pm-debug-toggle-after-change ()
  (interactive)
  (if pm-allow-after-change-hook
      (progn
        (message "after-change disabled")
        (setq pm-allow-after-change-hook nil))
    (message "after-change enabled")
    (setq pm-allow-after-change-hook t)))

(defun pm-debug-toggle-post-command ()
  (interactive)
  (if pm-allow-post-command-hook
      (progn
        (message "post-command disabled")
        (setq pm-allow-post-command-hook nil))
    (message "post-command enabled")
    (setq pm-allow-post-command-hook t)))

(defun pm-debug-toggle-all ()
  (interactive)
  (if pm-allow-fontification
      (progn
        (message "fontificaiton, after-chnage and command-hook disabled")
        (setq pm-allow-fontification nil
              pm-allow-after-change-hook nil
              pm-allow-post-command-hook nil))
    (message "fontificaiton, after-change and command-hook enabled")
    (setq pm-allow-fontification t
          pm-allow-after-change-hook t
          pm-allow-post-command-hook t)))

(defun pm-debug-fontify-current-span ()
  (interactive)
  (let ((span (pm-get-innermost-span))
        (pm-allow-fontification t))
    (poly-lock-fontify-region (nth 1 span) (nth 2 span))))

(defun pm-debug-fontify-last-font-lock-error ()
  (interactive)
  (let ((reg (pm--debug-get-last-fl-error))
        (pm-allow-fontification t))
    (if reg
        (progn
          ;; (pm-debug-blink-region (car reg) (cdr reg) 2)
          (poly-lock-fontify-region (car reg) (cdr reg)))
      (message "No last font-lock errors found"))))

(defun pm--debug-get-last-fl-error ()
  (with-current-buffer (messages-buffer)
     (goto-char (point-max))
     (when (re-search-backward "(poly-lock-fontify-region \\([0-9]+\\) \\([0-9]+\\))" nil t)
       (cons (string-to-number (match-string 1))
             (string-to-number (match-string 2))))))

(defun pm-debug-highlight-last-font-lock-error-region ()
  (interactive)
  (let ((reg (pm--debug-get-last-fl-error)))
    (if reg
        (progn
          (goto-char (car reg))
          (recenter)
          (move-overlay pm--inverse-video-overlay (car reg) (cdr reg) (current-buffer))
          (message "Region %s" reg))
      (message "No last font-lock errors found"))))

(defvar pm-debug-relevant-functions-alist
  '((polymode-initialization . (pm-initialize pm--mode-setup pm--common-setup
                                pm--get-chunkmode-buffer-create))
    (poly-lock . (poly-lock-mode poly-lock-fontify-region
                                 poly-lock-fontification-function
                                 poly-lock-after-change
                                 poly-lock-refontify
                                 poly-lock--fontify-region-original))
    (jit-loc . (jit-lock-refontify jit-lock-mode jit-lock-fontify-now))
    (font-lock . (;; font-lock-mode turn-on-font-lock-if-desired
                  turn-on-font-lock
                  font-lock-after-change-function
                  font-lock-default-fontify-region
                  font-lock-fontify-syntactically-region
                  font-lock-extend-region-wholelines
                  font-lock-extend-region-multiline
                  font-lock-fontify-syntactic-keywords-region
                  font-lock-fontify-keywords-region
                  font-lock-unfontify-region
                  font-lock-fontify-region font-lock-flush
                  font-lock-fontify-buffer font-lock-ensure))
    (methods . (pm-select-buffer pm-get-buffer-create))
    (select . (pm-get-innermost-span pm-map-over-spans))
    (insert . (self-insert-command))))

(defun pm-debug-trace-background-1 (fn)
  (interactive (trace--read-args "Trace function in background: "))
  (unless (symbolp fn)
    (error "can trace symbols only"))
  (unless (get fn 'cl--class)
    (trace-function-background fn nil
                               '(lambda ()
                                  (format " [buf:%s pos:%s type:%s (%f)]"
                                          (current-buffer) (point)
                                          (get-text-property (point) :pm-span-type)
                                          (float-time))))))

(defun pm-debug-trace-relevant-functions (&optional group)
  "GROUP is either a string or a list of functions to trace.
If string, it must b an entry in
`pm-debug-relevant-functions-alist'."
  (interactive)
  (require 'trace)
  (if (and group (listp group))
      (mapc #'pm-debug-trace-background-1 group)
   (let* ((groups (append '("*ALL*") (mapcar #'car pm-debug-relevant-functions-alist)))
          (group-name (or group (completing-read "Trace group: " groups nil t))))
     (if (equal group-name "*ALL*")
         (mapc (lambda (group)
                 (mapc #'pm-debug-trace-background-1
                       (assoc group pm-debug-relevant-functions-alist)))
               (cdr groups))
       (mapc #'pm-debug-trace-background-1
             (assoc (intern group-name) pm-debug-relevant-functions-alist))))))

(defun pm-debug-trace-functions-by-regexp (regexp)
  "Trace all functions whose name matched REGEXP."
  (cl-loop for sym being the symbols
           when (and (fboundp sym)
                     (not (eq sym 'pm-debug-trace-background-1)))
           when (string-match regexp (symbol-name sym))
           do (pm-debug-trace-background-1 sym)))

(defvar pm-debug-relevant-variables '(fontification-functions
                                      font-lock-flush-function
                                      font-lock-ensure-function
                                      font-lock-fontify-region-function
                                      font-lock-fontify-buffer-function
                                      font-lock-unfontify-region-function
                                      font-lock-unfontify-buffer-function
                                      post-command-hook
                                      indent-line-function))

(defun pm-debug-print-relevant-variables ()
  (interactive)
  (let ((buff (get-buffer-create "*polymode-vars*"))
        (vars (mapcar (lambda (v) (cons v (buffer-local-value v (current-buffer))))
                      pm-debug-relevant-variables))
        (cbuff (current-buffer)))
    (require 'pp)
    (with-current-buffer buff
      (goto-char (point-max))
      (insert "===============================================================\n")
      (insert (format "relevant vars in buffer: %s\n" cbuff))
      (insert (pp-to-string vars))
      (toggle-truncate-lines -1))
    (display-buffer buff)))

(defun pm-debug-untrace-relevant-functions ()
  (interactive)
  (require 'trace)
  (let* ((groups (append `("*ALL*") (mapcar #'car pm-debug-relevant-functions-alist)))
         (group-name (completing-read "Trace group: " groups nil t)))
    (if (equal group-name "*ALL*")
        (mapc (lambda (group)
                (mapc #'untrace-function (assoc group pm-debug-relevant-functions-alist)))
              (cdr groups))
      (mapc #'untrace-function (assoc groups pm-debug-relevant-functions-alist)))))

(defun pm-debug-blink-region (start end &optional delay)
  (move-overlay pm--inverse-video-overlay start end (current-buffer))
  (run-with-timer (or delay 0.4) nil (lambda () (delete-overlay pm--inverse-video-overlay))))

(defun pm-debug-map-over-spans-and-highlight ()
  (interactive)
  (pm-map-over-spans (lambda ()
                       (let ((start (nth 1 *span*))
                             (end (nth 2 *span*)))
                         (pm-debug-blink-region start end)
                         (sit-for 1)))
                     (point-min) (point-max) nil nil t))

(defun pm--highlight-span (&optional hd-matcher tl-matcher)
  (interactive)
  (let* ((hd-matcher (or hd-matcher (oref pm/chunkmode :head-reg)))
         (tl-matcher (or tl-matcher (oref pm/chunkmode :tail-reg)))
         (span (pm--span-at-point hd-matcher tl-matcher)))
    (pm-debug-blink-region (nth 1 span) (nth 2 span))
    (message "span: %s" span)))

(defun pm-debug-run-over-check ()
  (interactive)
  (goto-char (point-min))
  (let ((start (current-time))
        (count 1))
    (pm-switch-to-buffer)
    (while (< (point) (point-max))
      (setq count (1+ count))
      (forward-char)
      (pm-switch-to-buffer))
    (let ((elapsed  (float-time (time-subtract (current-time) start))))
      (message "elapsed: %s  per-char: %s" elapsed (/ elapsed count)))))

(defun pm-dbg (msg &rest args)
  (let ((cbuf (current-buffer))
        (cpos (point)))
   (with-current-buffer (get-buffer-create "*pm-dbg*")
     (save-excursion
       (goto-char (point-max))
       (insert "\n")
       (insert (apply 'format (concat "%f [%s at %d]: " msg)
                      (float-time) cbuf cpos args))))))

(provide 'polymode-debug)
