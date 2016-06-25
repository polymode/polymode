
;; `font-lock-mode' call graph:
;; -> font-lock-function <- we are replacing this with `poly-lock-mode'
;;   -> font-lock-default-function
;;     -> font-lock-mode-internal
;;        -> font-lock-turn-on-thing-lock
;;           -> font-lock-turn-on-thing-lock
;;             -> (setq font-lock-flush-function jit-lock-refontify)
;;             -> (setq font-lock-ensure-function jit-lock-fontify-now)
;;             -> (setq font-lock-fontify-buffer-function jit-lock-refontify)
;;             -> (jit-lock-register #'font-lock-fontify-region)
;;               -> (add-hook 'jit-lock-functions #'font-lock-fontify-region nil t)
;;               -> jit-lock-mode

(require 'polymode-core)
(require 'polymode-compat)

(defvar poly-lock-fontification-in-progress nil)
(defvar-local poly-lock-mode nil)
(defvar-local poly-lock--fontify-region-original nil)

(eval-when-compile
  (defmacro with-buffer-prepared-for-poly-lock (&rest body)
    "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
    (declare (debug t))
    `(let ((inhibit-point-motion-hooks t))
       (with-silent-modifications
         ,@body))))

(defun poly-lock-no-jit-lock-in-polymode-buffers (orig-fun arg)
  "Don't activate `jit-lock-mode' when in `polymode' buffers.
We are reusing some of the jit-lock functionality but don't want
to activate jit-lock."
  (unless (and polymode-mode pm/polymode)
    (funcall orig-fun arg)))
(pm-around-advice 'jit-lock-mode #'poly-lock-no-jit-lock-in-polymode-buffers)

(defun poly-lock-mode (arg)
  ;; value of `font-lock-function' in polymode buffers
  (unless polymode-mode
    (error "Trying to (de)activate `poly-lock-mode' in a non-polymode buffer (%s)" (current-buffer)))
  (setq poly-lock-mode arg)

  (if arg
      (progn
        ;; a lot of the following is inspired by what jit-lock does in
        ;; `font-lock-turn-on-thing-lock'

        (setq-local font-lock-support-mode 'poly-lock-mode)
        (setq-local font-lock-dont-widen t)

        ;; re-use jit-lock registration. Some minor modes (adaptive-wrap)
        ;; register extra functionality.
        (jit-lock-register 'font-lock-fontify-region)

        ;; don't allow other functions
        (setq-local fontification-functions '(poly-lock-fontification-function))

        (setq-local font-lock-flush-function 'poly-lock-refontify)
        (setq-local font-lock-ensure-function 'poly-lock-fontify-now)
        (setq-local font-lock-fontify-buffer-function 'poly-lock-refontify)

        ;; There are some more, jit-lock doesn't change those, neither do we:
        ;; font-lock-unfontify-region-function (defaults to font-lock-default-unfontify-region)
        ;; font-lock-unfontify-buffer-function (defualts to font-lock-default-unfontify-buffer)

        ;; Don't fontify eagerly (and don't abort if the buffer is large). This
        ;; is probably not needed but let it be.
        (setq-local font-lock-fontified t)

        ;; Now we can finally call `font-lock-default-function' because
        ;; `font-lock-support-mode' is set to "unrecognizible" value, only core
        ;; font-lock setup happens.
        (font-lock-default-function arg)

        ;; Must happen after call to `font-lock-default-function'
        (remove-hook 'after-change-functions 'font-lock-after-change-function t)
        (remove-hook 'after-change-functions 'jit-lock-after-change t)
        (add-hook 'after-change-functions 'poly-lock-after-change nil t)

        ;; Reusing jit-lock var becuase mode populate it directly. We are using
        ;; this in `poly-lock-after-change' below. Taken from `jit-lock
        ;; initialization.
        (add-hook 'jit-lock-after-change-extend-region-functions
                  'font-lock-extend-jit-lock-region-after-change
                  nil t))

    (remove-hook 'after-change-functions 'poly-lock-after-change t)
    (remove-hook 'fontification-functions 'poly-lock-fontification-function t))
  (current-buffer))

(defun poly-lock-fontification-function (start)
  "The only function in `fontification-functions'.
This is the entry point called by the display engine. START is
defined in `fontification-functions'. This function is has the
same scope as `jit-lock-function'."
  (unless pm-initialization-in-progress
    (if pm-allow-fontification
        (when (and poly-lock-mode (not memory-full))
          (unless (input-pending-p)
            (let ((end (or (text-property-any start (point-max) 'fontified t)
                           (point-max))))
              (when (< start end)
                (poly-lock-fontify-now start end)))))
      (with-buffer-prepared-for-poly-lock
       (put-text-property start (point-max) 'fontified t)))))

(defun poly-lock-fontify-now (beg end &optional verbose)
  "Polymode font-lock fontification function.
Fontifies chunk-by chunk within the region BEG END."
  (unless (or poly-lock-fontification-in-progress
              pm-initialization-in-progress)
    (let* ((font-lock-dont-widen t)
           (pmarker (point-marker))
           (dbuffer (current-buffer))
           ;; Fontification in one buffer can trigger fontification in another
           ;; buffer. Particularly, this happens when new indirect buffers are
           ;; created and `normal-mode' triggers font-lock in those buffers. We
           ;; avoid this by dynamically binding
           ;; `poly-lock-fontification-in-progress' and un-setting
           ;; `fontification-functions' in case re-display suddenly decides to
           ;; fontify something else in other buffer.
           (poly-lock-fontification-in-progress t)
           (fontification-functions nil))
      (save-restriction
        (widen)
        (save-excursion
          (pm-map-over-spans
           (lambda ()
             (with-buffer-prepared-for-poly-lock
              (let ((sbeg (nth 1 *span*))
                    (send (nth 2 *span*)))
                (when (> send sbeg)
                  (if  (not (and font-lock-mode font-lock-keywords))
                      ;; when no font-lock, set to t to avoid repeated calls
                      ;; from display engine
                      (put-text-property sbeg send 'fontified t)
                    (let ((new-beg (max sbeg beg))
                          (new-end (min send end)))
                      (condition-case-unless-debug err
                          ;; (if (oref pm/chunkmode :font-lock-narrow)
                          ;;     (pm-with-narrowed-to-span *span*
                          ;;       (font-lock-unfontify-region new-beg new-end)
                          ;;       (font-lock-fontify-region new-beg new-end verbose))
                          ;;   (font-lock-unfontify-region new-beg new-end)
                          ;;   (font-lock-fontify-region new-beg new-end verbose))
                          (if (oref pm/chunkmode :font-lock-narrow)
                              (pm-with-narrowed-to-span *span*
                                (jit-lock-fontify-now new-beg new-end))
                            (jit-lock-fontify-now new-beg new-end))
                        (error (message "(poly-lock-fontify-now %s %s) -> (%s %s %s %s): %s "
                                        beg end poly-lock--fontify-region-original new-beg new-end verbose
                                        (error-message-string err))))
                      ;; even if failed set to t
                      (put-text-property new-beg new-end 'fontified t))

                    (pm--adjust-chunk-face sbeg send (pm-get-adjust-face pm/chunkmode)))))))
           beg end))))
    (current-buffer)))

(defun poly-lock-refontify (&optional beg end)
  "Force refontification of the region BEG..END.
END is extended to the next chunk separator. This function is
pleased in `font-lock-flush-function' and
`font-lock-ensure-function'"
  (when (and pm-allow-fontification
             (not poly-lock-fontification-in-progress)
             (not pm-initialization-in-progress))
    (with-buffer-prepared-for-poly-lock
     (save-restriction
       (widen)
       (cond ((and beg end)
              (setq end (cdr (pm-get-innermost-range end))))
             (beg
              (setq end (cdr (pm-get-innermost-range beg))))
             (t
              (setq beg (point-min)
                    end (point-max))))
       (put-text-property beg end 'fontified nil)))))

(defun poly-lock-after-change (beg end old-len)
  "Mark changed region as not fontified after change.
Installed on `after-change-functions'."
  (save-match-data
    (when (and poly-lock-mode
               pm-allow-after-change-hook
               (not memory-full))
      (let ((jit-lock-start beg)
            (jit-lock-end end)
            ;; useful info for tracing
            (gl-beg end)
            (gl-end beg)
            exp-error)
        (save-excursion
          (condition-case err
              ;; set jit-lock-start and jit-lock-end locally
              (run-hook-with-args 'jit-lock-after-change-extend-region-functions
                                  beg end old-len)
            (error (message "(poly-lock-after-change:jl-expand (%s %s %s)): %s"
                            beg end old-len (error-message-string err))
                   (setq jit-lock-start beg
                         jit-lock-end end)))
          (setq beg (min beg jit-lock-start)
                end (max end jit-lock-end))
          (pm-map-over-spans
           (lambda ()
             (with-buffer-prepared-for-poly-lock
              (let ((sbeg (nth 1 *span*))
                    (send (nth 2 *span*)))
                (save-restriction
                  (widen)
                  (setq gl-beg (min gl-beg (max jit-lock-start sbeg))
                        gl-end (max gl-beg jit-lock-end send))
                  (put-text-property gl-beg gl-end 'fontified nil)))))
           beg end nil nil nil 'no-cache)
          (cons gl-beg gl-end))))))

(provide 'poly-lock)
