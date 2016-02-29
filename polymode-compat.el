;;; COMPATIBILITY and FIXES

(require 'polymode-common)
(require 'advice nil t)

(defgroup polymode-compat nil
  "Polymode compatibility settings."
  :group 'polymode)



;;; Various Wrappers for Around Advice

(defvar *span* nil)

(defun pm-override-output-position (orig-fun &rest args)
  "Restrict returned value of ORIG-FUN to fall into the current span.
*span* in `pm-map-over-spans` has precedence over span at point.'"
  (if (and polymode-mode pm/polymode)
      (let ((range (or (pm-span-to-range *span*)
                       (pm-get-innermost-range)))
            (pos (apply orig-fun args)))
        (and pos
             (min (max pos (car range))
                  (cdr range))))
    (apply orig-fun args)))

(defun pm-override-output-cons (orig-fun &rest args)
  "Restrict returned (beg . end) of ORIG-FUN to fall into the current span.
*span* in `pm-map-over-spans` has precedence over span at point.'"
  (if (and polymode-mode pm/polymode)
      (let ((range (or (pm-span-to-range *span*)
                       (pm-get-innermost-range)))
            (be (apply orig-fun args)))
        (and be
             (cons (min (max (car be) (car range))
                        (cdr range))
                   (max (min (cdr be) (cdr range))
                        (car range)))))
    (apply orig-fun args)))

(defun pm-substitute-beg-end (orig-fun beg end &rest args)
  "Execute orig-fun with first two arguments limited to current span.
*span* in `pm-map-over-spans` has precedence over span at point."
  (if (and polymode-mode pm/polymode)
      (let* ((pos (if (and (<= (point) end) (>=  (point) beg))
                      (point)
                    end))
             (range (or (pm-span-to-range *span*)
                        (pm-get-innermost-range pos)))
             (new-beg (max beg (car range)))
             (new-end (min end (cdr range))))
        (apply orig-fun new-beg new-end args))
    (apply orig-fun beg end args)))

(eval-when-compile
  (defmacro pm-with-narrowed-to-span (&rest body)
    (declare (indent 0) (debug body))
    `(save-restriction
       (pm-narrow-to-span *span*)
       (let ((pm--restrict-widen t))
         ,@body))))

(defun pm-execute-narowed-to-span (orig-fun &rest args)
  "Execute ORIG-FUN narrowed to the current span.
*span* in `pm-map-over-spans` has precedence over span at point."
  (if (and polymode-mode pm/polymode)
      (pm-with-narrowed-to-span
        (apply orig-fun args))
    (apply orig-fun args)))

(defun pm-execute-widened (orig-fun &rest args)
  "Execute ORIG-FUN widened"
  (if (and polymode-mode pm/polymode)
      (save-restriction
        (widen)
        (apply orig-fun args))
    (apply orig-fun args)))


;;; Syntax

(defun pm-execute-syntax-propertize-narrowed-to-span (orig-fun &rest args)
  "Execute `syntax-propertize' narrowed to the current span.
Don't throw errors, but give relevant messages instead."
  (if (and polymode-mode pm/polymode)
      (condition-case err
          (pm-with-narrowed-to-span
            (apply orig-fun args))
        (error (message "(syntax-propertize %s): %s" (car args)
                        (error-message-string err))))
    (apply orig-fun args)))

(when (fboundp 'advice-add)
  (advice-add 'syntax-propertize :around 'pm-execute-syntax-propertize-narrowed-to-span))


;;; Flyspel
(defun pm--flyspel-dont-highlight-in-chunkmodes (beg end poss)
  (or (get-text-property beg :pm-span-type)
      (get-text-property end :pm-span-type)))


;;; C/C++/Java
(when (fboundp 'advice-add)
  (advice-add 'c-before-context-fl-expand-region :around #'pm-override-output-cons)
  (advice-add 'c-state-semi-safe-place :around #'pm-override-output-position)
  ;; (advice-remove 'c-state-semi-safe-place #'pm-override-output-position)

  ;; c-font-lock-fontify-region calls it directly
  ;; (advice-add 'font-lock-default-fontify-region :around #'pm-substitute-beg-end)
  (advice-add 'c-determine-limit :around #'pm-execute-narowed-to-span))


;;; Core Font Lock
(defun pm-check-for-real-change-in-extend-multiline (fun)
  "Fix `font-lock-extend-region-multiline' which causes infloops on point-max.
Propagate only real change."
  ;; fixme: report this ASAP!
  (let ((obeg font-lock-beg)
        (oend font-lock-end)
        (change (funcall fun)))
    (and change
         (not (eq obeg font-lock-beg))
         (not (eq oend font-lock-end)))))

(when (fboundp 'advice-add)
  (advice-add 'font-lock-extend-region-multiline :around #'pm-check-for-real-change-in-extend-multiline))


;;; Editing
(when (fboundp 'advice-add)
  (advice-add 'fill-paragraph :around #'pm-execute-narowed-to-span))


;;; EVIL

(defcustom polymode-evil-states '(normal insert emacs)
  "States of evil-mode to be preserved when switching indirect buffers."
  :type '(symbol)
  :group 'polymode-compat)

(defun polymode-switch-buffer-keep-evil-state-maybe (old-buffer new-buffer)
  (when (and (boundp 'evil-state)
             evil-state)
    (let ((old-state (buffer-local-value 'evil-state old-buffer))
          (new-state (buffer-local-value 'evil-state new-buffer)))
      (when (and (not (eq old-state new-state))
                 (member old-state polymode-evil-states))
        (with-current-buffer new-buffer
          (evil-change-state old-state))))))

(eval-after-load 'evil-core
  '(add-hook 'polymode-switch-buffer-hook 'polymode-switch-buffer-keep-evil-state-maybe))


(provide 'polymode-compat)
