;;; COMPATIBILITY and FIXES


;;; Various Wrappers for Around Advice

(defvar *span* nil)

(defun pm-override-output-position (orig-fun &rest args)
  "Restrict returned value of ORIG-FUN to fall into the current span.
*span* in `pm-map-over-spans` has precedence over span at point.'"
  (if (and polymode-mode pm/chunkmode)
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
  (if (and polymode-mode pm/chunkmode)
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
  (if (and polymode-mode pm/chunkmode)
	  (let* ((pos (if (and (<= (point) end) (>=  (point) beg))
					  (point)
					end))
			 (range (or (pm-span-to-range *span*)
                        (pm-get-innermost-range pos)))
			 (new-beg (max beg (car range)))
			 (new-end (min end (cdr range))))
		(apply orig-fun new-beg new-end args))
	(apply orig-fun beg end args)))

(defun pm-execute-narowed-to-span (orig-fun &rest args)
  "Execute ORIG-FUN narrowed to the current span.
*span* in `pm-map-over-spans` has precedence over span at point."
  (if (and polymode-mode pm/chunkmode)
	  ;; fixme: extract into a macro (pm-with-narrowed-to-span))
	  (save-restriction
		(pm-narrow-to-span *span*)
		(let ((pm--restrict-widen t))
		  (apply orig-fun args)))
	(apply orig-fun args)))



;;; Flyspel
(defun pm--flyspel-dont-highlight-in-chunkmodes (beg end poss)
  (or (get-text-property beg :pm-span-type)
      (get-text-property end :pm-span-type)))


;;; EIEIO
(defvar object-name)
(defun pm--object-name (object)
  (cond ((fboundp 'eieio--object-name)
         (eieio--object-name object))
        ((fboundp 'eieio-object-name)
         (eieio-object-name object))
        (t (aref object object-name))))


;;; C/C++/Java
(when (string< "24.4" emacs-version)
  (advice-add 'c-before-context-fl-expand-region :around #'pm-override-output-cons)
  (advice-add 'c-state-semi-safe-place :around #'pm-override-output-position)
  ;; (advice-remove 'c-state-semi-safe-place #'pm-override-output-position)

  ;; c-font-lock-fontify-region calls it directly
  ;; (advice-add 'font-lock-default-fontify-region :around #'pm-substitute-beg-end)
  (advice-add 'c-determine-limit :around #'pm-execute-narowed-to-span))


;; (when (string< "24.4" emacs-version)
;;   (advice-add 'font-lock-default-fontify-region :around #'pm-execute-beg-end-function-narowed-to-span))

;; (advice-remove 'font-lock-default-fontify-region #'pm-execute-beg-end-function-narowed-to-span)

;; (defun pm-execute-with-restricted-widen (orig &rest args)
;;   "Restrict `widen' inside this function to the current span."
;;   (let ((pm--restrict-widen t))
;; 	(apply orig args)))

;; (defun pm-execute-beg-end-function-narowed-to-span (orig beg end &rest args)
;;   "Execute this function narrowed to the current span."
;;   (if (and polymode-mode pm/chunkmode)
;; 	  ;; fixme: extract into a macro (pm-with-narrowed-to-span))
;; 	  (save-restriction
;; 		(let (pm--restrict-widen)
;; 		  (widen)
;; 		  (pm-narrow-to-span (pm-get-innermost-span end))
;; 		  (let ((pm--restrict-widen t)
;; 				(font-lock-dont-widen t)
;; 				(new-beg (min (point-max) (max (point-min) beg)))
;; 				(new-end (max (point-min) (min (point-max) end))))
;; 			(apply orig new-beg new-end args))))
;; 	(apply orig beg end args)))

;; (defvar pm--restrict-widen nil)
;; (defun pm-restrict-widen (orig-widen)
;;   "Don't widen beyond current span.
;; Many modes (e.g. c-mode) don't respect
;; `font-lock-dont-widen'. Thus we need to perform a deeper surgery
;; with this advice."
;;   (if (and polymode-mode
;; 		   pm--restrict-widen)
;; 	  (let ((beg (get-text-property (point) :pm-span-beg))
;; 			(end (get-text-property (point) :pm-span-end)))
;; 		(funcall orig-widen)
;; 		(when (and beg end)
;; 		  (narrow-to-region beg end)))
;; 	(funcall orig-widen)))

;; (defun pm-adjust-pps-beginning (orig-fun from to &rest args)
;;   "Change `parse-partial-sexp' beg to span beginning if needed"
;;   (if polymode-mode
;; 	  (let ((beg (get-text-property to :pm-span-beg))
;; 			(pm--restrict-widen nil))
;; 		(save-restriction
;; 		  (widen)
;; 		  (apply orig-fun (max from (or beg 0)) to args)))
;; 	(apply orig-fun from to args)))

;; (when (string< "24.4" emacs-version)
;;   (advice-add 'widen :around #'pm-restrict-widen)
;;   (advice-add 'parse-partial-sexp :around #'pm-adjust-pps-beginning))
;; (advice-remove 'parse-partial-sexp #'pm-adjust-pps-beginning)
;; (advice-remove 'widen #'pm--restrict-widen)

;; (when (string< "24.4" emacs-version)
;;   (with-eval-after-load "cc-mode"

;; 	(defun c-before-context-fl-expand-region (beg end)
;; 	  ;; Expand the region (BEG END) as specified by
;; 	  ;; `c-before-context-fontification-functions'.  Return a cons of the bounds
;; 	  ;; of the new region.
;; 	  (save-restriction
;; 		(widen)
;; 		(save-excursion
;; 		  (let ((new-beg beg) (new-end end) new-region)
;; 			(mapc (lambda (fn)
;; 					(setq new-region (funcall fn new-beg new-end))
;; 					(setq new-beg (car new-region) new-end (cdr new-region)))
;; 				  c-before-context-fontification-functions)
;; 			new-region))))

;; 	;; this function is a real pain in the ass. consider removing altogether
;; 	(advice-add 'c-after-change :around #'pm-execute-beg-end-function-narowed-to-span)))

(provide 'polymode-compat)
