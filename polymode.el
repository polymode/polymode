;;; polymode.el --- support for multiple major modes
;; Author: Vitalie Spinu

(require 'font-lock)
(require 'imenu)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)
(add-to-list 'load-path "~/VC/polymode/")
(require 'polymode-classes)
(require 'polymode-methods)
(require 'polymode-modes)

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

(defvar pm/fontify-region-original nil
  "Fontification function normally used by the buffer's major mode.
Used internaly to cahce font-lock-fontify-region-function.  Buffer local.")
(make-variable-buffer-local 'multi-fontify-region-original)


(defvar pm/base-mode nil)
(make-variable-buffer-local 'pm/base-mode)

(defvar pm/default-submode nil)
(make-variable-buffer-local 'pm/default-submode)

(defvar pm/config nil)
(make-variable-buffer-local 'pm/config)



(defcustom polymode-prefix-key "\M-n"
  "Prefix key for the litprog mode keymap.
Not effective after loading the LitProg library."
  :group 'litprog
  :type '(choice string vector))

(defvar polymode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map polymode-prefix-key
      (let ((map (make-sparse-keymap)))
	(define-key map "\C-n" 'litprog-next-header)
	(define-key map "\C-p" 'litprog-previous-header)
	;; (define-key map "\M-n" 'litprog-goto-next)
	;; (define-key map "\M-p" 'litprog-goto-prev)
	;; (define-key map "c" 'litprog-code-next)
	;; (define-key map "C" 'litprog-code-prev)
	;; (define-key map "d" 'litprog-doc-next)
	;; (define-key map "D" 'litprog-doc-prev)
        ;; Use imenu.
        ;; 	(define-key map "\C-g" 'litprog-goto-chunk)
        (define-key map "\M-k" 'litprog-kill-chunk)
        (define-key map "\M-K" 'litprog-kill-chunk-pair)
        (define-key map "\M-m" 'litprog-mark-chunk)
        (define-key map "\M-M" 'litprog-mark-chunk-pair)
        (define-key map "\M-n" 'litprog-narrow-to-chunk)
        (define-key map "\M-N" 'litprog-narrow-to-chunk-pair)
        (define-key map "\C-t" 'litprog-toggle-narrowing)
	(define-key map "\M-i" 'litprog-new-chunk)
	(define-key map "."	'litprog-select-backend)
	(define-key map "$"	'litprog-display-process)
	;; (if (bound-and-true-p litprog-electric-<)
	;;     (define-key litprog-mode-map "<" #'litprog-electric-<))
	;; (if (bound-and-true-p litprog-electric-@)
	;;     (define-key litprog-mode-map "@" #'litprog-electric-@))
	map))
    (define-key map [menu-bar LitProG]
      (cons "LitProG"
	    (let ((map (make-sparse-keymap "LitProG")))
              (define-key-after map [goto-prev]
		'(menu-item "Next chunk header" litprog-next-header))
	      (define-key-after map [goto-prev]
		'(menu-item "Previous chunk header" litprog-previous-header))
	      (define-key-after map [mark]
		'(menu-item "Mark chunk" litprog-mark-chunk))
	      (define-key-after map [kill]
		'(menu-item "Kill chunk" litprog-kill-chunk))
	      (define-key-after map [new]
		'(menu-item "New chunk" litprog-new-chunk))
	      map)))
    map)
  "A keymap for LitProG mode.")


(defsubst pm/base-buffer ()
  ;; fixme: redundant with :base-buffer 
  "Return base buffer of current buffer, or the current buffer if it's direct."
  (or (buffer-base-buffer (current-buffer))
      (current-buffer)))

;; VS[26-08-2012]: Dave's comment:
;; It would be nice to cache the results of this on text properties,
;; but that probably won't work well if chunks can be nested.  In that
;; case, you can't just mark everything between delimiters -- you have
;; to consider other possible regions between them.  For now, we do
;; the calculation each time, scanning outwards from point.
(defun pm/get-inermost-span (&optional pos)
  "Apply pm/get-span on every element of submodes slot of pm/config object.
Return a cons (submode . span), for which START is closest to
POS (and before it); i.e. the innermost span.  POS defaults to
point."
  ;; fixme: base should be last, to take advantage of the submodes computation
  (let ((smodes (cons (oref pm/config :base-submode) 
                      (oref pm/config :inner-submodes)))
	(start (point-min))
	(end (point-max))
	(pos (or pos (point)))
        submode span val)
    (save-restriction
      (widen)
      (dolist (sm smodes)
	(setq val (pm/get-span sm pos))
	(if (and val (>= (nth 1 val) start))
	    (setq submode sm
                  span val
		  start (nth 1 val)
		  end (nth 2 val)))))
    (unless (and (<= start end) (<= pos end) (>= pos start))
      (error "Bad polymode selection: %s, %s"
	     (list start end) pos))
    ;; fixme: why is this here?
    (if (= start end)
	(setq end (1+ end)))
    (cons (if (car span) ; submodes can compute the base span by returning nil
              submode 
            (oref pm/config :base-submode))
          span)))

(defun pm/map-over-spans (beg end fun)
  "For all spans between BEG and END, execute FUN.
FUN is a function of no args.  It is executed with point at the
beginning of the span and with the buffer narrowed to the
span."
  (save-excursion
    (save-window-excursion ;; why is th
      (goto-char beg)
      (while (< (point) end)
        (let ((span (pm/get-inermost-span)))
          (pm/select-buffer (car span) (cadr span))
          (save-restriction
            (pm/narrow-to-span span)
            (funcall fun)
            (goto-char (point-max)))
          )))))

(defun pm/narrow-to-span (&optional span)
  "Narrow to current chunk."
  (interactive)
  (if (boundp 'syntax-ppss-last)
      (setq syntax-ppss-last nil)) ;; fix me: why not let bind?
  (unless (= (point-min) (point-max))
    (let ((span (or span
                    (pm/get-inermost-span))))
      (if span 
          (apply #'narrow-to-region (cddr span))
        (error "No span found")))))

(defun pm/fontify-region (beg end loudly)
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
	 deactivate-mark)
    (font-lock-unfontify-region beg end)
    (save-restriction
      (widen)
      (pm/map-over-spans beg end
                         (lambda ()
                           ;; (message  "%s %s (%s %s) point: %s"
                           ;;           (current-buffer) major-mode (point-min) (point-max) (point))
                           (if (and font-lock-mode font-lock-keywords)
                               (funcall pm/fontify-region-original
                                        (point-min) (point-max) loudly)))))
    ;; In case font-lock isn't done for some mode:
    (put-text-property beg end 'fontified t)
    (when (and (not modified) (buffer-modified-p))
      (set-buffer-modified-p nil))))



;;; internals

(defun polymode-select-buffer ()
  "Select the appropriate (indirect) buffer corresponding to point's context.
This funciton is placed in local post-command hook."
  ;; (condition-case error
  (let ((span (pm/get-inermost-span)))
    (pm/select-buffer (car span) (cadr span)))
  ;; (error
  ;;  (message "%s" (error-message-string error))))
  )

(defun pm--select-buffer (buffer)
  (unless (eq buffer (current-buffer))
    (when (buffer-live-p buffer)
      (let* ((point (point))
             (window-start (window-start))
             (visible (pos-visible-in-window-p))
             (oldbuf (current-buffer)))
        (switch-to-buffer buffer)
        (bury-buffer oldbuf)
        (goto-char point)
        ;; Avoid the display jumping around.
        (when visible
          (set-window-start (get-buffer-window buffer t) window-start))
        ))))

(defvar pm--killed-once nil)
(make-variable-buffer-local 'pm--killed-once)

(defun pm--create-indirect-buffer (mode)
  "Create indirect buffer with major MODE and initialize appropriately.

This is a low lever function which must be called, one way or
another from `pm/install' method. Among other things store
`pm/config' from the base buffer (must always exist!) in
the newly created buffer.

Return newlly created buffer."
  (unless   (buffer-local-value 'pm/config (pm/base-buffer))
    (error "`pm/config' not found in the base buffer %s" (pm/base-buffer)))
  
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
  (if (eq 'autoload (car-safe (indirect-function mode)))
      (with-temp-buffer
        (insert "Local Variables:\nmode: fundamental\nEnd:\n")
        (funcall mode)
        (hack-local-variables)))

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
           (coding buffer-file-coding-system))
      ;; todo: see clone-indirect-buffer for other stuff to clone.
      (with-current-buffer new-buffer
        (let ((polymode-mode t)) ;;major-modes might check it 
          (funcall mode))
        ;; Now we can make it local:
        (set (make-local-variable 'polymode-mode) t)
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
        (if (featurep 'advice)
            (ad-with-originals (hack-one-local-variable)
              (pm/hack-local-variables))
          (pm/hack-local-variables))

        (setq pm/config config)

        (pm--setup-buffer)
        ;; Avoid the uniqified name for the indirect buffer in the
        ;; mode line.
        ;; (setq mode-line-buffer-identification
        ;;       (propertized-buffer-identification base-name))

        (setq buffer-file-coding-system coding)
        ;; For benefit of things like VC
        (setq buffer-file-name file)
        (vc-find-file-hook))
      new-buffer)))


(defun pm--create-indirect-buffer-maybe (mode)
  (or (loop for bf in (oref pm/config :buffers)
            when (eq mode (buffer-local-value 'major-mode bf) )
            return bf)
      (pm--create-indirect-buffer mode)))

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

;; (oref pm-submode/noweb-R :tail-mode)
;; (oref pm-submode/noweb-R :buffer)
;; (oref pm-submode/noweb-R :head-buffer)
;; (pm--set-submode-buffer pm-submode/noweb-R 'tail (current-buffer))


(defun pm--setup-buffer (&optional buffer)
  ;; general buffer setup, should work for indirect and base buffers alike
  (with-current-buffer (or buffer (current-buffer))
    (setq pm/fontify-region-original
          font-lock-fontify-region-function)
    (set (make-local-variable 'font-lock-fontify-region-function)
         #'pm/fontify-region)

    ;; Don't let parse-partial-sexp get fooled by syntax outside
    ;; the chunk being fontified.
    (set (make-local-variable 'font-lock-dont-widen) t)
    
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
    (add-hook 'post-command-hook 'polymode-select-buffer nil t)
    (object-add-to-list pm/config :buffers (current-buffer))))

;;;; HACKS
;; VS[26-08-2012]: Dave Love's hack. See commentary above.
(defun pm/hack-local-variables ()
  "Like `hack-local-variables', but ignore `mode' items."
  (let ((late-hack (symbol-function 'hack-one-local-variable)))
    (fset 'hack-one-local-variable
	  (lambda (var val)
	    (unless (eq var 'mode)
	      (funcall late-hack var val))))
    (unwind-protect
	(hack-local-variables)
      (fset 'hack-one-local-variable late-hack))))


(define-derived-mode noweb-mode2 fundamental-mode "Noweb"
  "Mode for editing noweb documents.
Supports differnt major modes for doc and code chunks using multi-mode."
  ;; Extract values of local variables now, so we know the doc and
  ;; code modes.  Nullify noweb-mode2 while we process possible
  ;; `mode: noweb' line to avoid infinite regress.
  (flet ((noweb-mode2 ()))
    ;; (hack-local-variables)
    (setq pm/config (clone pm-config/noweb))
    ;; (when pm/base-mode
    ;;   (oset pm/config :base-mode pm/base-mode))
    ;; (when pm/default-submode
    ;;   (oset pm/config :default-submode pm/default-submode))
    )
  (polymode-minor-mode))

;; Used to propagate the bindings to the indirect buffers.
(define-minor-mode polymode-minor-mode
  "Polymode minor mode, used to make everything work."
  nil " PM" polymode-mode-map
  ;; at this stage pm/config should be set locally
  (pm/initialize pm/config))

(add-to-list 'auto-mode-alist '("Tnw" . noweb-mode2))
  

(provide 'polymode)
