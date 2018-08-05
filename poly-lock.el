
;; FONT-LOCK COMPONENTS:
;;
;; All * functions are lazy in poly-lock and jit-lock because they just mark
;; 'fontified nil.
;;
;;  fontification-functions ->                                           jit-lock-function  / poly-lock-function
;; *font-lock-flush  ->           font-lock-flush-function  ->           jit-lock-refontify / poly-lock-refontify
;;  font-lock-ensure ->           font-lock-ensure-function ->           jit-lock-fontify-now/poly-lock-fontify-now
;;  font-lock-fontify-region ->   font-lock-fontify-region-function ->   font-lock-default-fontify-region
;; *font-lock-fontify-buffer ->   font-lock-fontify-buffer-function ->   jit-lock-refontify / poly-lock-refontify
;;  font-lock-unfontify-region -> font-lock-unfontify-region-function -> font-lock-default-unfontify-region
;;  font-lock-unfontify-buffer -> font-lock-unfontify-buffer-function -> font-lock-default-unfontify-buffer

;; Jit-lock components:
;; fontification-functions (called by display engine)
;;   --> jit-lock-function
;;     --> jit-lock-fontify-now (or deferred through timer/text-properties)
;;       --> jit-lock--run-functions
;;         --> jit-lock-functions (font-lock-fontify-region bug-reference-fontify)
;;

;; Poly-lock components:
;; fontification-functions
;;   --> poly-lock-function
;;    --> poly-lock-fontify-now
;;      --> jit-lock-fontify-now

;; `font-lock-mode' call graph:
;; -> font-lock-function <---- replaced by `poly-lock-mode'
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

(defvar poly-lock-verbose nil)
(defvar poly-lock-fontification-in-progress nil)
(defvar-local poly-lock-mode nil)

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
  (unless (or polymode-mode pm/polymode)
    (funcall orig-fun arg)))
(pm-around-advice 'jit-lock-mode #'poly-lock-no-jit-lock-in-polymode-buffers)

(defun poly-lock-mode (arg)
  "This is the value of `font-lock-function' in all polymode buffers.
Mode activated when `font-lock' is toggled."
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
        (setq-local fontification-functions '(poly-lock-function))

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

        ;; Reusing jit-lock var becuase modes populate it directly. We are using
        ;; this in `poly-lock-after-change' below. Taken from `jit-lock
        ;; initialization.
        (add-hook 'jit-lock-after-change-extend-region-functions
                  'font-lock-extend-jit-lock-region-after-change
                  nil t))

    (remove-hook 'after-change-functions 'poly-lock-after-change t)
    (remove-hook 'fontification-functions 'poly-lock-function t))
  (current-buffer))

(defun poly-lock-function (start)
  "The only function in `fontification-functions' in polymode buffers.
This is the entry point called by the display engine. START is
defined in `fontification-functions'. This function has the same
scope as `jit-lock-function'."
  (when poly-lock-verbose
    (message "(poly-lock-function %d)" start))
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

(defun poly-lock-fontify-now (beg end &optional _verbose)
  "Polymode main fontification function.
Fontifies chunk-by chunk within the region BEG END."
  (when poly-lock-verbose
    (message "(poly-lock-fontify-now %d %d)" beg end))
  (unless (or poly-lock-fontification-in-progress
              pm-initialization-in-progress)
    (let* ((font-lock-dont-widen t)
           (font-lock-extend-region-functions nil)
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
                      (when poly-lock-verbose
                        (message "(jit-lock-fontify-now %d %d)" new-beg new-end)
                        (message "(font-lock-default-fontify-region %d %d nil)" new-beg new-end))
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
                        (error
                         (message "(poly-lock-fontify-now %s %s [span %d %d %s]) -> (%s %s %s): %s"
                                  beg end sbeg send (current-buffer)
                                  font-lock-fontify-region-function new-beg new-end
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
  (when poly-lock-verbose
    (message "(poly-lock-refontify %d %d)" beg end))
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
  "Mark changed region with 'fontified nil.
Installed in `after-change-functions' and behaves similarly to
`jit-lock-after-change' in what it calls
`jit-lock-after-change-extend-region-functions' in turn but with
the buffer narrowed to the relevant spans."
  (save-excursion
    (save-match-data
      (when (and poly-lock-mode
                 pm-allow-after-change-hook
                 (not memory-full))
        (when poly-lock-verbose
          (message "(poly-lock-after-change %d %d %d) [%s]" beg end old-len (current-buffer)))
        (pm-map-over-spans
         (lambda ()
           (with-buffer-prepared-for-poly-lock
            (let ((sbeg (nth 1 *span*))
                  (send (nth 2 *span*)))
              (when poly-lock-verbose
                (message "span: %d-%d [%s]" sbeg send (current-buffer)))
              (pm-with-narrowed-to-span *span*
                (let ((jit-lock-start (max beg sbeg))
                      (jit-lock-end (min end send)))
                  (when (or (> beg sbeg) (< end send))
                    (condition-case err
                        ;; set jit-lock-start and jit-lock-end locally
                        (run-hook-with-args 'jit-lock-after-change-extend-region-functions
                                            jit-lock-start jit-lock-end old-len)
                      (error (message "(jl-after-change-extend-region-functions %s %s %s): %s"
                                      jit-lock-start jit-lock-end old-len (error-message-string err)))))
                  (setq jit-lock-start (max jit-lock-start sbeg)
                        jit-lock-end (min jit-lock-end send))
                  (when poly-lock-verbose
                    (message "jl-after-change-extend-region-functions -> %d-%d [%s]"
                             jit-lock-start jit-lock-end (current-buffer)
                             jit-lock-after-change-extend-region-functions))
                  (put-text-property jit-lock-start jit-lock-end 'fontified nil))))))
         beg end nil nil nil 'no-cache)))))

(provide 'poly-lock)
