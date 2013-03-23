;;; polymode.el --- support for multiple major modes
;; Author: Vitalie Spinu

(require 'font-lock)
(require 'imenu)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)
(require 'polymode-classes)
(require 'polymode-methods)

(defgroup polymode nil
  "Object oriented framework for multiple modes based on indirect buffers"
  :link '(emacs-commentary-link "polymode")
  :group 'tools)

(defgroup base-submodes nil
  "Base Submodes"
  :group 'polymode)

(defgroup submodes nil
  "Children Submodes"
  :group 'polymode)

(defvar polymode-select-mode-hook nil
  "Hook run after a different mode is selected.")

(defvar polymode-indirect-buffer-hook nil
  "Hook run by `pm/install-mode' in each indirect buffer.
It is run after all the indirect buffers have been set up.")

;; (defvar pm/base-mode nil)
;; (make-variable-buffer-local 'pm/base-mode)

;; (defvar pm/default-submode nil)
;; (make-variable-buffer-local 'pm/default-submode)

(defvar pm/config nil)
(make-variable-buffer-local 'pm/config)

(defvar pm/submode nil)
(make-variable-buffer-local 'pm/submode)

(defvar pm/type nil)
(make-variable-buffer-local 'pm/type)
;; (defvar pm/overlay nil)
;; (make-variable-frame-local 'pm/overlay)

(defcustom polymode-prefix-key "\M-n"
  "Prefix key for the polymode mode keymap.
Not effective after loading the polymode library."
  :group 'polymode
  :type '(choice string vector))

(defvar polymode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map polymode-prefix-key
      (let ((map (make-sparse-keymap)))
	(define-key map "\C-n" 'polymode-next-header)
	(define-key map "\C-p" 'polymode-previous-header)
	;; (define-key map "\M-n" 'polymode-goto-next)
	;; (define-key map "\M-p" 'polymode-goto-prev)
	;; (define-key map "c" 'polymode-code-next)
	;; (define-key map "C" 'polymode-code-prev)
	;; (define-key map "d" 'polymode-doc-next)
	;; (define-key map "D" 'polymode-doc-prev)
        ;; Use imenu.
        ;; 	(define-key map "\C-g" 'polymode-goto-chunk)
        (define-key map "\M-k" 'polymode-kill-chunk)
        (define-key map "\M-K" 'polymode-kill-chunk-pair)
        (define-key map "\M-m" 'polymode-mark-chunk)
        (define-key map "\M-M" 'polymode-mark-chunk-pair)
        (define-key map "\M-n" 'polymode-narrow-to-chunk)
        (define-key map "\M-N" 'polymode-narrow-to-chunk-pair)
        (define-key map "\C-t" 'polymode-toggle-narrowing)
	(define-key map "\M-i" 'polymode-new-chunk)
	(define-key map "."	'polymode-select-backend)
	(define-key map "$"	'polymode-display-process)
	;; (if (bound-and-true-p polymode-electric-<)
	;;     (define-key polymode-mode-map "<" #'polymode-electric-<))
	;; (if (bound-and-true-p polymode-electric-@)
	;;     (define-key polymode-mode-map "@" #'polymode-electric-@))
	map))
    (define-key map [menu-bar Polymode]
      (cons "Polymode"
	    (let ((map (make-sparse-keymap "Polymode")))
              (define-key-after map [goto-prev]
		'(menu-item "Next chunk header" polymode-next-header))
	      (define-key-after map [goto-prev]
		'(menu-item "Previous chunk header" polymode-previous-header))
	      (define-key-after map [mark]
		'(menu-item "Mark chunk" polymode-mark-chunk))
	      (define-key-after map [kill]
		'(menu-item "Kill chunk" polymode-kill-chunk))
	      (define-key-after map [new]
		'(menu-item "New chunk" polymode-new-chunk))
	      map)))
    map)
  "The default minor mode keymap that is active in all polymode
  modes.")


(defsubst pm/base-buffer ()
  ;; fixme: redundant with :base-buffer 
  "Return base buffer of current buffer, or the current buffer if it's direct."
  (or (buffer-base-buffer (current-buffer))
      (current-buffer)))

;; ;; VS[26-08-2012]: Dave's comment:
;; ;; It would be nice to cache the results of this on text properties,
;; ;; but that probably won't work well if chunks can be nested.  In that
;; ;; case, you can't just mark everything between delimiters -- you have
;; ;; to consider other possible regions between them.  For now, we do
;; ;; the calculation each time, scanning outwards from point.
(defun pm/get-innermost-span (&optional pos)
  (pm/get-span pm/config pos))

(defvar pm--can-narrow? t)
(defun pm/map-over-spans (fun beg end &optional dont-narrow)
  "For all spans between BEG and END, execute FUN.
FUN is a function of no args.  It is executed with point at the
beginning of the span and with the buffer narrowed to the
span.

During the call of FUN, a dynamically bound variable *span* hold
the current innermost span."
  (save-excursion
    (save-window-excursion 
      (goto-char beg)
      (while (< (point) end)
        (let ((*span* (pm/get-innermost-span)))
          (pm/select-buffer (car (last *span*)) *span*) ;; object and type
          (save-restriction
            (unless dont-narrow
              (pm/narrow-to-span *span*))
            (funcall fun)
            (goto-char (nth 2 *span*))))))))

(defun pm/narrow-to-span (&optional span)
  "Narrow to current chunk."
  (interactive)
  (unless (= (point-min) (point-max))
    (let ((span (or span
                    (pm/get-innermost-span))))
      (if span
          (let ((min (nth 1 span))
                (max (nth 2 span)))
            (when (boundp 'syntax-ppss-last)
              (setq syntax-ppss-last
                    (cons (point-min)
                          (list 0 nil (point-min) nil nil nil 0 nil nil nil))))
            (narrow-to-region min max))
        (error "No span found")))))

(defvar pm--fontify-region-original nil
  "Fontification function normally used by the buffer's major mode.
Used internaly to cahce font-lock-fontify-region-function.  Buffer local.")
(make-variable-buffer-local 'multi-fontify-region-original)

(defun pm/fontify-region (beg end &optional verbose)
  "Polymode font-lock fontification function.
Fontifies chunk-by chunk within the region.
Assigned to `font-lock-fontify-region-function'.

A fontification mechanism should call
`font-lock-fontify-region-function' (`jit-lock-function' does
that). If it does not, the fontification will probably be screwed
in polymode buffers."
  (let* ((modified (buffer-modified-p))
         (buffer-undo-list t)
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
         (font-lock-dont-widen t)
         (buff (current-buffer))
	 deactivate-mark)
    ;; (with-silent-modifications
    (font-lock-unfontify-region beg end)
    (save-excursion
      (save-restriction
        (widen)
        (pm/map-over-spans
         (lambda ()
           (when (and font-lock-mode font-lock-keywords)
             (let ((sbeg (nth 1 *span*))
                   (send (nth 2 *span*)))
               ;; (dbg (point-min) (point-max) (point))
               (pm--adjust-chunk-overlay sbeg send buff) ;; set in original buffer!
               (when parse-sexp-lookup-properties
                 (pm--comment-region 1 sbeg))
               ;; (dbg sbeg send)
               (unwind-protect 
                   (if (oref (nth 3 *span*) :font-lock-narrow)
                       (save-restriction
                         (narrow-to-region sbeg send)
                         (funcall pm--fontify-region-original
                                  (max sbeg beg) (min send end) verbose)
                         )
                     (funcall pm--fontify-region-original
                              (max sbeg beg) (min send end) verbose))
                 (when parse-sexp-lookup-properties
                   (pm--uncomment-region 1 sbeg))
                 ))))
         beg end 'dont-narrow))
      (put-text-property beg end 'fontified t)
      (unless modified
        (restore-buffer-modified-p nil)))))


;;; internals
(defun pm--get-available-mode (mode)
  "Check if MODE symbol is defined and is a valid function.
If so, return it, otherwise return 'fundamental-mode with a
warnign."
  (if (fboundp mode)
      mode
    (message "Cannot find " mode " function, using 'fundamental-mode instead")
    'fundamental-mode))

(defvar pm--ignore-post-command-hook nil)
(defun pm--restore-ignore ()
  (setq pm--ignore-post-command-hook nil))

(defvar polymode-highlight-chunks t)

(defun polymode-select-buffer ()
  "Select the appropriate (indirect) buffer corresponding to point's context.
This funciton is placed in local post-command hook."
  (condition-case error
      (unless pm--ignore-post-command-hook
        (let ((*span* (pm/get-innermost-span))
              (pm--can-move-overlays t))
          (pm/select-buffer (car (last *span*)) *span*)
          (pm--adjust-chunk-overlay (nth 1 *span*) (nth 2 *span*))))
    (error (message "polymode error: %s"
                    (error-message-string error)))))
  


(defun pm/transform-color-value (color prop)
  "Darken or lighten a specific COLOR multiplicatively by PROP.

On dark backgrounds, values of PROP > 1 generate lighter colors
than COLOR and < 1, darker. On light backgrounds, do it the other
way around.

Colors are in hex RGB format #RRGGBB

   (pm/transform-color-value (face-background 'default) 1.1)
"
  (let* ((st (substring color 1))
         (RGB (list (substring st 0 2)
                    (substring st 2 4)
                    (substring st 4 6))))
    (when (eq (frame-parameter nil 'background-mode) 'light)
      (setq prop (/ 1 prop)))
    (when (< prop 0)
      (message "background value should be non-negative" )
      (setq prop 1))
    (concat "#"
            (mapconcat (lambda (n)
                         (format "%02x"
                                 (min 255 (max 0 (round (* prop (string-to-number n 16)))))))
                       RGB ""))))

(defun pm--adjust-chunk-overlay (beg end &optional buffer)
  ;; super duper internal function
  ;; should be used only after pm/select-buffer
  (when (eq pm/type 'body)
    (let ((background (oref pm/submode :background))) ;; in Current buffer !!
      (with-current-buffer (or buffer (current-buffer))
        (when background
          (let* ((OS (overlays-in  beg end))
                 (o (some (lambda (o) (and (overlay-get o 'polymode) o))
                          OS)))
            (if o
                (move-overlay o  beg end )
              (let ((o (make-overlay beg end nil nil t))
                    (face (if (numberp background)
                              (cons 'background-color
                                    (pm/transform-color-value (face-background 'default)
                                                              background))
                            background)))
                (overlay-put o 'polymode 'polymode-major-mode)
                (overlay-put o 'face face)
                (overlay-put o 'evaporate t)))))))))

(defun pm--adjust-visual-line-mode (new-vlm)
  (when (not (eq visual-line-mode vlm))
    (if (null vlm)
        (visual-line-mode -1)
      (visual-line-mode 1))))

;; move only in post-command hook, after buffer selection
(defvar pm--can-move-overlays nil)
(defun pm--move-overlays (new-buff)
  (when pm--can-move-overlays 
    (mapc (lambda (o)
            (move-overlay o (overlay-start o) (overlay-end o) new-buff))
          (overlays-in 1 (1+ (buffer-size))))))

(defun pm--select-buffer (buffer)
  (when (and (not (eq buffer (current-buffer)))
             (buffer-live-p buffer))
    (let* ((point (point))
           (window-start (window-start))
           (visible (pos-visible-in-window-p))
           (oldbuf (current-buffer))
           (vlm visual-line-mode)
           (ractive (region-active-p))
           (mkt (mark t)))
      (pm--move-overlays buffer)
      (switch-to-buffer buffer)
      (pm--adjust-visual-line-mode vlm)
      (bury-buffer oldbuf)
      ;; fixme: wha tis the right way to do this ... activate-mark-hook?
      (if (not ractive)
          (deactivate-mark)
        (set-mark mkt)
        (activate-mark))
      (goto-char point)
      ;; Avoid the display jumping around.
      (when visible
        (set-window-start (get-buffer-window buffer t) window-start))
      )))


(defun pm--setup-buffer (&optional buffer)
  ;; general buffer setup, should work for indirect and base buffers alike
  ;; assumes pm/config and pm/submode is already in place
  ;; return buffer
  (let ((buff (or buffer (current-buffer))))
    (with-current-buffer buff
      ;; Don't let parse-partial-sexp get fooled by syntax outside
      ;; the chunk being fontified.
      ;; font-lock, forward-sexp etc should see syntactic comments
      ;; (set (make-local-variable 'parse-sexp-lookup-properties) t)

      (set (make-local-variable 'font-lock-dont-widen) t)
      
      (when pm--dbg-fontlock 
        (setq pm--fontify-region-original
              font-lock-fontify-region-function)
        (set (make-local-variable 'font-lock-fontify-region-function)
             #'pm/fontify-region))

      (set (make-local-variable 'polymode-mode) t)
      (funcall (oref pm/config :minor-mode-name) t)



      ;; Indentation should first narrow to the chunk.  Modes
      ;; should normally just bind `indent-line-function' to
      ;; handle indentation.
      (when (and indent-line-function ; not that it should ever be nil...
                 (oref pm/submode :protect-indent-line-function))
        (set (make-local-variable 'indent-line-function)
             `(lambda ()
                (let ((span (pm/get-innermost-span)))
                  (unwind-protect
                      (save-restriction
                        (pm--comment-region  1 (nth 1 span))
                        (pm/narrow-to-span span)
                        (,indent-line-function))
                    (pm--uncomment-region 1 (nth 1 span)))))))

      ;; Kill the base buffer along with the indirect one; careful not
      ;; to infloop.
      ;; (add-hook 'kill-buffer-hook
      ;;           '(lambda ()
      ;;              ;; (setq kill-buffer-hook nil) :emacs 24 bug (killing
      ;;              ;; dead buffer triggers an error)
      ;;              (let ((base (buffer-base-buffer)))
      ;;                (if  base
      ;;                    (unless (buffer-local-value 'pm--killed-once base)
      ;;                      (kill-buffer base))
      ;;                  (setq pm--killed-once t))))
      ;;           t t)
      
      ;; This should probably be at the front of the hook list, so
      ;; that other hook functions get run in the (perhaps)
      ;; newly-selected buffer.
      
      (when pm--dbg-hook
        (add-hook 'post-command-hook 'polymode-select-buffer nil t))
      (object-add-to-list pm/config :buffers (current-buffer)))
    buff))

(defvar pm--killed-once nil)
(make-variable-buffer-local 'pm--killed-once)


;; adapted from org
;; (defun pm--clone-local-variables (from-buffer &optional regexp)
;;   "Clone local variables from FROM-BUFFER.
;; Optional argument REGEXP selects variables to clone."
;;   (mapc
;;    (lambda (pair)
;;      (and (symbolp (car pair))
;; 	  (or (null regexp)
;; 	      (string-match regexp (symbol-name (car pair))))
;;           (condition-case error ;; some special wars cannot be set directly, how to solve?
;;               (set (make-local-variable (car pair))
;;                    (cdr pair))
;;             ;; fixme: enable-multibyte-characters cannot be set, what are others?
;;             (error ;(message  "--dbg local set: %s" (error-message-string error))
;;                    nil))))
;;    (buffer-local-variables from-buffer)))

(defun pm--create-indirect-buffer (mode)
  "Create indirect buffer with major MODE and initialize appropriately.

This is a low lever function which must be called, one way or
another from `pm/install' method. Among other things store
`pm/config' from the base buffer (must always exist!) in
the newly created buffer.

Return newlly created buffer."
  (unless   (buffer-local-value 'pm/config (pm/base-buffer))
    (error "`pm/config' not found in the base buffer %s" (pm/base-buffer)))
  
  (setq mode (pm--get-available-mode mode))
  ;; VS[26-08-2012]: The following if is Dave Love's hack in multi-mode. Kept
  ;; here, because i don't really understand it.

  ;; This is part of a grim hack for lossage in AUCTeX, which
  ;; bogusly advises `hack-one-local-variable'.  This loses, due to
  ;; the way advice works, when we run `pm/hack-local-variables'
  ;; below -- there ought to be a way round this, probably with CL's
  ;; flet.  Any subsequent use of it then fails because advice has
  ;; captured the now-unbound variable `late-hack'...  Thus ensure
  ;; we've loaded the mode in advance to get any autoloads sorted
  ;; out.  Do it generally in case other modes have similar
  ;; problems.  [The AUCTeX stuff is in support of an undocumented
  ;; feature which is unnecessary and, anyway, wouldn't need advice
  ;; to implement.  Unfortunately the maintainer seems not to
  ;; understand the local variables mechanism and wouldn't remove
  ;; this.  To invoke minor modes, you should just use `mode:' in
  ;; `local variables'.]
  ;; (if (eq 'autoload (car-safe (indirect-function mode)))
  ;;     (with-temp-buffer
  ;;       (insert "Local Variables:\nmode: fundamental\nEnd:\n")
  ;;       (funcall mode)
  ;;       (hack-local-variables)))

  (with-current-buffer (pm/base-buffer)
    (let* ((config (buffer-local-value 'pm/config (current-buffer)))
           (new-name
            (generate-new-buffer-name 
             (format "%s[%s]" (buffer-name)
                     (replace-regexp-in-string "-mode" "" (symbol-name mode)))))
           (new-buffer (make-indirect-buffer (current-buffer)  new-name))
           ;; (hook pm/indirect-buffer-hook)
           (file (buffer-file-name))
           (base-name (buffer-name))
           (jit-lock-mode nil)
           (coding buffer-file-coding-system)
           (tbf (get-buffer-create "*pm-tmp*")))

      ;; do it in empty buffer to exclude all kind of font-lock issues
      ;; Or, is there a reliable way to deactivate font-lock temporarly?
      ;; (with-current-buffer tbf
      ;;   (let ((polymode-mode t)) ;;major-modes might check it
      ;;     (funcall mode)))
      (with-current-buffer new-buffer
        (let ((polymode-mode t)) ;;major-modes might check it
          (funcall mode))
        ;; clonning doesn't really work, local maps 
        ;; (pm--clone-local-variables tbf)
        ;; Now we can make it local:
        (setq polymode-major-mode mode)
        
        ;; VS[26-08-2012]: Dave Love's hack.
        ;; Use file's local variables section to set variables in
        ;; this buffer.  (Don't just copy local variables from the
        ;; base buffer because it may have set things locally that
        ;; we don't want in the other modes.)  We need to prevent
        ;; `mode' being processed and re-setting the major mode.
        ;; It all goes badly wrong if `hack-one-local-variable' is
        ;; advised.  The appropriate mechanism to get round this
        ;; appears to be `ad-with-originals', but we don't want to
        ;; pull in the advice package unnecessarily.  `flet'-like
        ;; mechanisms lose with advice because `fset' acts on the
        ;; advice anyway.
        ;; (if (featurep 'advice)
        ;;     (ad-with-originals (hack-one-local-variable)
        ;;       (pm/hack-local-variables))
        ;;   (pm/hack-local-variables))


        ;; Avoid the uniqified name for the indirect buffer in the
        ;; mode line.
        (when pm--dbg-mode-line
          (setq mode-line-buffer-identification
                (propertized-buffer-identification base-name)))
        (setq pm/config config)
        ;; (face-remap-add-relative 'default '(:background "#073642"))
        (setq buffer-file-coding-system coding)
        ;; For benefit of things like VC
        (setq buffer-file-name file)
        (vc-find-file-hook))
      new-buffer)))


(defvar polymode-major-mode nil)
(make-variable-buffer-local 'polymode-major-mode)

(defun pm--get-indirect-buffer-of-mode (mode)
  (loop for bf in (oref pm/config :buffers)
        when (and (buffer-live-p bf)
                  (eq mode (buffer-local-value 'polymode-major-mode bf)))
        return bf))

(defun pm--set-submode-buffer (obj type buff)
  (with-slots (buffer head-mode head-buffer tail-mode tail-buffer) obj
    (pcase (list type head-mode tail-mode)
      (`(body body ,(or `nil `body))
       (setq buffer buff
             head-buffer buff
             tail-buffer buff))
      (`(body ,_ body)
       (setq buffer buff
             tail-buffer buff))
      (`(body ,_ ,_ )
       (setq buffer buff))
      (`(head ,_ ,(or `nil `head))
       (setq head-buffer buff
             tail-buffer buff))
      (`(head ,_ ,_)
       (setq head-buffer buff))
      (`(tail ,_ ,(or `nil `head))
       (setq tail-buffer buff
             head-buffer buff))
      (`(tail ,_ ,_)
       (setq tail-buffer buff))
      (_ (error "type must be one of 'body 'head and 'tail")))))

(defun pm--get-submode-mode (obj type)
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
                    (eq head-mode 'base))
               (and (eq type 'tail)
                    (or (eq tail-mode 'base)
                        (and (null tail-mode)
                             (eq head-mode 'base)))))
           (oref (oref pm/config :base-submode) :mode))
          ((eq type 'head)
           (oref obj :head-mode))
          ((eq type 'tail)
           (oref obj :tail-mode))
          (t (error "type must be one of 'head 'tail 'body")))))

;; (oref pm-submode/noweb-R :tail-mode)
;; (oref pm-submode/noweb-R :buffer)
;; (progn
;;   (pm--set-submode-buffer pm-submode/noweb-R 'tail (current-buffer))
;;   (oref pm-submode/noweb-R :head-buffer))

;;;; HACKS
;; VS[26-08-2012]: Dave Love's hack. See commentary above.
;; (defun pm/hack-local-variables ()
;;   "Like `hack-local-variables', but ignore `mode' items."
;;   (let ((late-hack (symbol-function 'hack-one-local-variable)))
;;     (fset 'hack-one-local-variable
;; 	  (lambda (var val)
;; 	    (unless (eq var 'mode)
;; 	      (funcall late-hack var val))))
;;     (unwind-protect
;; 	(hack-local-variables)
;;       (fset 'hack-one-local-variable late-hack))))

;; Used to propagate the bindings to the indirect buffers.
(define-minor-mode polymode-minor-mode
  "Polymode minor mode, used to make everything work."
  nil " PM" polymode-mode-map)

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
  (let* ((hd-matcher (or hd-matcher (oref pm/submode :head-reg)))
         (tl-matcher (or tl-matcher (oref pm/submode :tail-reg)))
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
    (let ((elapsed  (time-to-seconds (time-subtract (current-time) start))))
    (message "elapsed: %s  per-char: %s" elapsed (/ elapsed count)))))
  


;; do not delete
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
                                     'polymode-comment 'end))
          )))))

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


;; ;; this one does't really work, text-properties are the same in all buffers
;; (defun pm--mark-buffers-except-current (beg end)
;;   ;; marke with syntact comments all the buffers except this on
;;   (dolist ((bf (oref pm/config :buffers)))
;;     (when (and (buffer-live-p bf)
;;                (not (eq bf (current-buffer))))
;;       (pm--comment-region beg end bf)
;;       ;; (put-text-property beg end 'fontified t)
;;       )))

;; (defun pm/fontify-region-simle (beg end &optional verbose)
;;   (with-silent-modifications
;;     (put-text-property beg end 'fontified t)))


(setq pm--dbg-mode-line nil
      pm--dbg-fontlock t
      pm--dbg-hook t)

(provide 'polymode)
