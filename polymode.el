;;; polymode.el --- support for multiple major modes
;; Author: Vitalie Spinu

;; todo:
;; See: http://www.russet.org.uk/blog/2979

(eval-when-compile
  (require 'cl))

(require 'font-lock)
(require 'color)

(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)

(load "polymode-classes")
(load "polymode-methods")
(load "polymode-export")
(load "polymode-weave")

(defgroup polymode nil
  "Object oriented framework for multiple modes based on indirect buffers"
  :link '(emacs-commentary-link "polymode")
  :group 'tools)

(defgroup polymode-configs nil
  "Polymode Configuration Objects"
  :group 'polymode)

(defgroup polymode-basemodes nil
  "Polymode Base Submode Objects"
  :group 'polymode)

(defgroup polymode-chunkmodes nil
  "Polymode Submode Objects"
  :group 'polymode)

(defvar polymode-select-mode-hook nil
  "Hook run after a different mode is selected.")

(defvar polymode-indirect-buffer-hook nil
  "Hook run by `pm/install-mode' in each indirect buffer.
It is run after all the indirect buffers have been set up.")

(defvar pm/config nil)
(make-variable-buffer-local 'pm/config)

(defvar pm/submode nil)
(make-variable-buffer-local 'pm/submode)

(defvar pm/type nil)
(make-variable-buffer-local 'pm/type)

(defcustom polymode-prefix-key "\M-n"
  "Prefix key for the polymode mode keymap.
Not effective after loading the polymode library."
  :group 'polymode
  :type '(choice string vector))

(defvar polymode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map polymode-prefix-key
      (let ((map (make-sparse-keymap)))
        ;; navigation
	(define-key map "\C-n" 'polymode-next-chunk)
	(define-key map "\C-p" 'polymode-previous-chunk)
        (define-key map "\C-\M-n" 'polymode-next-chunk-same-type)
	(define-key map "\C-\M-p" 'polymode-previous-chunk-same-type)
        ;; chunk manipulation
        (define-key map "\M-k" 'polymode-kill-chunk)
        (define-key map "\M-m" 'polymode-mark-or-extend-chunk)
        (define-key map "\C-t" 'polymode-toggle-chunk-narrowing)
	(define-key map "\M-i" 'polymode-insert-new-chunk)
        ;; backends
        (define-key map "e" 'polymode-export)
        (define-key map "E" 'polymode-set-exporter)
        (define-key map "w" 'polymode-weave)
        (define-key map "W" 'polymode-set-weaver)
        (define-key map "t" 'polymode-tangle)
        (define-key map "T" 'polymode-set-tangler)
        ;; todo: add polymode-goto-process-buffer
	map))
    (define-key map [menu-bar Polymode]
      (cons "Polymode"
	    (let ((map (make-sparse-keymap "Polymode")))
              (define-key-after map [next]
		'(menu-item "Next chunk" polymode-next-chunk))
	      (define-key-after map [previous]
		'(menu-item "Previous chunk" polymode-previous-chunk))
              (define-key-after map [next-same]
	        '(menu-item "Next chunk same type" polymode-next-chunk-same-type))
	      (define-key-after map [previous-same]
	        '(menu-item "Previous chunk same type" polymode-previous-chunk-same-type))
	      (define-key-after map [mark]
	        '(menu-item "Mark or extend chunk" polymode-mark-or-extend-chunk))
	      (define-key-after map [kill]
	        '(menu-item "Kill chunk" polymode-kill-chunk))
	      (define-key-after map [insert]
	        '(menu-item "Insert new chunk" polymode-insert-new-chunk))
	      map)))
    map)
  "The default minor mode keymap that is active in all polymode
  modes.")


;;; COMMANDS
(defun polymode-next-chunk (&optional N)
  "Go COUNT chunks forwards.
Return, how many chucks actually jumped over."
  (interactive "p")
  (let* ((sofar 0)
         (back (< N 0))
         (beg (if back (point-min) (point)))
         (end (if back (point) (point-max)))
         (N (if back (- N) N)))
    (condition-case nil
        (pm/map-over-spans
         (lambda ()
           (unless (memq (car *span*) '(head tail))
             (when (>= sofar N)
               (signal 'quit nil))
             (setq sofar (1+ sofar))))
         beg end nil back)
      (quit (when (looking-at "\\s *$")
              (forward-line))))
    sofar))

;;fixme: problme with long chunks .. point is recentered
;;todo: merge into next-chunk
(defun polymode-previous-chunk (&optional N)
  "Go COUNT chunks backwards .
Return, how many chucks actually jumped over."
  (interactive "p")
  (polymode-next-chunk (- N)))
  
(defun polymode-next-chunk-same-type (&optional N)
  "Go to next COUNT chunk.
Return, how many chucks actually jumped over."
  (interactive "p")
  (let* ((sofar 0)
         (back (< N 0))
         (beg (if back (point-min) (point)))
         (end (if back (point) (point-max)))
         (N (if back (- N) N))
         this-type this-class)
    (condition-case nil
        (pm/map-over-spans
         (lambda ()
           (unless (memq (car *span*) '(head tail))
             (when (and (equal this-class (object-name (car (last *span*))))
                        (eq this-type (car *span*)))
               (setq sofar (1+ sofar)))
             (unless this-class
               (setq this-class (object-name (car (last *span*)))
                     this-type (car *span*)))
             (when (>= sofar N)
               (signal 'quit nil))))
         beg end nil back)
      (quit (when (looking-at "\\s *$")
              (forward-line))))
    sofar))

(defun polymode-previous-chunk-same-type (&optional N)
  "Go to previus COUNT chunk.
Return, how many chucks actually jumped over."
  (interactive "p")
  (polymode-next-chunk-same-type (- N)))

(defun pm--kill-span (types)
  (let ((span (pm/get-innermost-span)))
    (when (memq (car span) types)
      (delete-region (nth 1 span) (nth 2 span)))))

(defun polymode-kill-chunk ()
  "Kill current chunk"
  (interactive)
  (pcase (pm/get-innermost-span)
    (`(,(or `nil `base) ,beg ,end ,_) (delete-region beg end))
    (`(body ,beg ,end ,_)
     (goto-char beg)
     (pm--kill-span '(body))
     (pm--kill-span '(head tail))
     (pm--kill-span '(head tail)))
    (`(tail ,beg ,end ,_)
     (if (eq beg (point-min))
         (delete-region beg end)
       (goto-char (1- beg))
       (polymode-kill-chunk)))
    (`(head ,_ ,end ,_)
     (goto-char end)
     (polymode-kill-chunk))
    (_ (error "canoot find chunk to kill"))))

(defun polymode-kill-chunk ()
  "Kill current chunk"
  (interactive)
  (pcase (pm/get-innermost-span)
    (`(,(or `nil `base) ,beg ,end ,_) (delete-region beg end))
    (`(body ,beg ,end ,_)
     (goto-char beg)
     (pm--kill-span '(body))
     (pm--kill-span '(head tail))
     (pm--kill-span '(head tail)))
    (`(tail ,beg ,end ,_)
     (if (eq beg (point-min))
         (delete-region beg end)
       (goto-char (1- beg))
       (polymode-kill-chunk)))
    (`(head ,_ ,end ,_)
     (goto-char end)
     (polymode-kill-chunk))
    (_ (error "canoot find chunk to kill"))))


(defun polymode-toggle-chunk-narrowing ()
  "Toggle narrowing of the current chunk."
  (interactive)
  (if (buffer-narrowed-p)
      (progn (widen) (recenter))
    (pcase (pm/get-innermost-span)
      (`(head ,_ ,end ,_)
       (goto-char end)
       (pm/narrow-to-span))
      (`(tail ,beg ,end ,_)
       (if (eq beg (point-min))
           (error "Invalid chunk")
         (goto-char (1- beg))
         (pm/narrow-to-span)))
      (_ (pm/narrow-to-span)))))

(defun polymode-mark-or-extend-chunk ()
  (interactive)
  (error "Not implemented yet"))

(defun polymode-insert-new-chunk ()
  (interactive)
  (error "Not implemented yet"))


;;; CORE
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

(defvar pm--ignore-post-command-hook nil)
(defun pm--restore-ignore ()
  (setq pm--ignore-post-command-hook nil))

;; This function is for debug convenience only in order to avoid limited debug
;; context in polymode-select-buffer
(defun pm--sel-buf ()
  (unless pm--ignore-post-command-hook
    (let ((*span* (pm/get-innermost-span))
          (pm--can-move-overlays t))
      (pm/select-buffer (car (last *span*)) *span*))))

(defun polymode-select-buffer ()
  "Select the appropriate (indirect) buffer corresponding to point's context.
This funciton is placed in local post-command hook."
  (condition-case error
      (pm--sel-buf)
    (error (message "polymode error: %s"
                    (error-message-string error)))))

(defvar pm--can-narrow? t)

(defun pm/map-over-spans (fun beg end &optional count backward?)
  "For all spans between BEG and END, execute FUN.
FUN is a function of no args. It is executed with point at the
beginning of the span and with the buffer narrowed to the
span. If COUNT is non-nil, jump at most that many times. If
BACKWARD? is non-nil, map backwards.
 
During the call of FUN, a dynamically bound variable *span* holds
the current innermost span."
  (let ((nr 0)
        *span*)
    ;; (save-excursion
    ;;   (save-window-excursion
    (goto-char (if backward? end beg))
    (while (and (if backward?
                    (> (point) beg)
                  (< (point) end))
                (or (null count)
                    (< nr count)))
      (setq *span* (pm/get-innermost-span)
            nr (1+ nr))
      (pm/select-buffer (car (last *span*)) *span*) ;; object and type
      (goto-char (nth 1 *span*))
      (funcall fun)
      (if backward?
          (goto-char (max 1 (1- (nth 1 *span*)))) ;; enter previous chunk
        (goto-char (nth 2 *span*))))));))

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
(make-variable-buffer-local 'pm--fontify-region-original)

(defvar pm--indent-line-function-original nil
  "Used internally toprotect buffer's `indent-line-function'.")
(make-variable-buffer-local 'pm--indent-line-function-original)

(defvar pm--syntax-begin-function-original nil)
(make-variable-buffer-local 'pm--syntax-begin-function-original)


(defun pm/fontify-region (beg end &optional verbose)
  "Polymode font-lock fontification function.
Fontifies chunk-by chunk within the region.
Assigned to `font-lock-fontify-region-function'.

A fontification mechanism should call
`font-lock-fontify-region-function' (`jit-lock-function' does
that). If it does not, the fontification will probably be screwed
in polymode buffers."
  (let* ((buffer-undo-list t)
	 (inhibit-point-motion-hooks t)
         (font-lock-dont-widen t)
         (buff (current-buffer)))
    (with-silent-modifications
      (font-lock-unfontify-region beg end)
      (save-excursion
        (save-window-excursion
          (pm/map-over-spans
           (lambda ()
             (let ((sbeg (nth 1 *span*))
                   (send (nth 2 *span*)))
               (when (and font-lock-mode font-lock-keywords)
                 (when parse-sexp-lookup-properties
                   (pm--comment-region 1 sbeg))
                 (condition-case err
                     (if (oref pm/submode :font-lock-narrow)
                         (save-restriction
                           ;; fixme: optimization oportunity: Cache chunk state
                           ;; in text properties. For big chunks font-lock
                           ;; fontifies it by smaller segments, thus
                           ;; pm/fontify-region is called multiple times per
                           ;; chunk and spans are computed each time.
                           (narrow-to-region sbeg send)
                           (funcall pm--fontify-region-original
                                    (max sbeg beg) (min send end) verbose))
                       (funcall pm--fontify-region-original
                                (max sbeg beg) (min send end) verbose))
                   (error (message "polymode font-lock error: %s (beg: %s end: %s)"
                                   (error-message-string err) beg end)))
                 (when parse-sexp-lookup-properties
                   (pm--uncomment-region 1 sbeg)))
               (pm--adjust-chunk-face sbeg send (pm/get-adjust-face pm/submode))
               ;; might be needed by external applications like flyspell
               ;; fixme: this should be in a more generic place like pm/get-span
               (put-text-property sbeg send 'chunkmode
                                  (object-of-class-p pm/submode 'pm-chunkmode))
               ;; even if failed, set to t to avoid infloop
               (put-text-property beg end 'fontified t)))
           beg end)
          ;; needed to avoid moving last fontified buffer to second place
          (bury-buffer))))))

(defun pm/syntax-begin-function ()
  (goto-char
   (max (cadr (pm/get-innermost-span))
        (if pm--syntax-begin-function-original
            (save-excursion
              (funcall pm--syntax-begin-function-original)
              (point))
          (point-min)))))


;;; INTERNALS
(defun pm--get-available-mode (mode)
  "Check if MODE symbol is defined and is a valid function.
If so, return it, otherwise return 'fundamental-mode with a
warnign."
  (if (fboundp mode)
      mode
    (message "Cannot find " mode " function, using 'fundamental-mode instead")
    'fundamental-mode))

(defun pm--lighten-background (prop)
  ;; if > lighten on dark backgroun. Oposite on light.
  (color-lighten-name (face-background 'default) 
                      (if (eq (frame-parameter nil 'background-mode) 'light)
                          (- prop) ;; darken
                        prop)))

(defun pm--adjust-chunk-face (beg end face)
  ;; propertize 'face of the region by adding chunk specific configuration
  (interactive "r")
  (when face
    (with-current-buffer (current-buffer)
      (let ((face (or (and (numberp face)
                           (cons 'background-color
                                 (pm--lighten-background face)))
                      face))
            (pchange nil))
        (while (not (eq pchange end))
          (setq pchange (next-single-property-change beg 'face nil end))
          (put-text-property beg pchange 'face
                             `(,face ,@(get-text-property beg 'face)))
          (setq beg pchange))))))

;; (defun pm--adjust-chunk-overlay (beg end &optional buffer)
;;   ;; super duper internal function
;;   ;; should be used only after pm/select-buffer
;;   (when (eq pm/type 'body)
;;     (let ((background (oref pm/submode :background))) ;; in Current buffer !!
;;       (with-current-buffer (or buffer (current-buffer))
;;         (when background
;;           (let* ((OS (overlays-in  beg end))
;;                  (o (some (lambda (o) (and (overlay-get o 'polymode) o))
;;                           OS)))
;;             (if o
;;                 (move-overlay o  beg end )
;;               (let ((o (make-overlay beg end nil nil t))
;;                     (face (or (and (numberp background)
;;                                    (cons 'background-color
;;                                          (pm--lighten-background background)))
;;                               background)))
;;                 (overlay-put o 'polymode 'polymode-major-mode)
;;                 (overlay-put o 'face face)
;;                 (overlay-put o 'evaporate t)))))))))

(defun pm--adjust-visual-line-mode (new-vlm)
  (when (not (eq visual-line-mode vlm))
    (if (null vlm)
        (visual-line-mode -1)
      (visual-line-mode 1))))

;; move only in post-command hook, after buffer selection
(defvar pm--can-move-overlays nil)
(defun pm--move-overlays-to (new-buff)
  (when pm--can-move-overlays 
    (mapc (lambda (o)
            (move-overlay o (overlay-start o) (overlay-end o) new-buff))
          (overlays-in 1 (1+ (buffer-size))))))

(defun pm--select-buffer (buffer)
  (when (and (not (eq buffer (current-buffer)))
             (buffer-live-p buffer))
    (let ((point (point))
          (window-start (window-start))
          (visible (pos-visible-in-window-p))
          (oldbuf (current-buffer))
          (vlm visual-line-mode)
          (ractive (region-active-p))
          (mkt (mark t))
          (bis buffer-invisibility-spec))
      (pm--move-overlays-to buffer)
      (switch-to-buffer buffer)
      (setq buffer-invisibility-spec bis)
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
        (set-window-start (get-buffer-window buffer t) window-start)))))

(defun pm--transfer-vars-from-base ()
  (let ((bb (pm/base-buffer)))
    (dolist (var '(buffer-file-name))
      (set var (buffer-local-value var bb)))))

(defun pm--setup-buffer (&optional buffer)
  ;; General buffer setup, should work for indirect and base buffers
  ;; alike. Assumes pm/config and pm/submode is already in place. Return buffer.
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
             #'pm/fontify-region)
        (setq pm--syntax-begin-function-original
              (or syntax-begin-function ;; Emacs > 23.3
                  font-lock-beginning-of-syntax-function))
        (set (make-local-variable 'syntax-begin-function)
             #'pm/syntax-begin-function))

      (set (make-local-variable 'polymode-mode) t)

      ;; Indentation should first narrow to the chunk.  Modes
      ;; should normally just bind `indent-line-function' to
      ;; handle indentation.
      (when (and indent-line-function ; not that it should ever be nil...
                 (oref pm/submode :protect-indent-line))
        (setq pm--indent-line-function-original indent-line-function)
        (set (make-local-variable 'indent-line-function) 'pm/indent-line))

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
      
      (when pm--dbg-hook
        (add-hook 'post-command-hook 'polymode-select-buffer nil t))
      (object-add-to-list pm/config '-buffers (current-buffer)))
    buff))

(defvar pm--killed-once nil)
(make-variable-buffer-local 'pm--killed-once)
(defvar polymode-mode nil)
(defvar pm--ib-prefix "")

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

  (with-current-buffer (pm/base-buffer)
    (let* ((config (buffer-local-value 'pm/config (current-buffer)))
           (new-name
            (generate-new-buffer-name 
             (format "%s%s[%s]" pm--ib-prefix (buffer-name)
                     (replace-regexp-in-string "-mode" "" (symbol-name mode)))))
           (new-buffer (make-indirect-buffer (current-buffer)  new-name))
           ;; (hook pm/indirect-buffer-hook)
           (file (buffer-file-name))
           (base-name (buffer-name))
           (jit-lock-mode nil)
           (coding buffer-file-coding-system))

      ;; (dbg (current-buffer) file)
      ;; (backtrace)

      (with-current-buffer new-buffer
        (let ((polymode-mode t)) ;;major-modes might check it
          (funcall mode))
        (setq polymode-major-mode mode)
        
        ;; Avoid the uniqified name for the indirect buffer in the mode line.
        (when pm--dbg-mode-line
          (setq mode-line-buffer-identification
                (propertized-buffer-identification base-name)))
        (setq pm/config config)
        (setq buffer-file-coding-system coding)
        (setq buffer-file-name file)
        (vc-find-file-hook))
      new-buffer)))


(defvar polymode-major-mode nil)
(make-variable-buffer-local 'polymode-major-mode)

(defun pm--get-indirect-buffer-of-mode (mode)
  (loop for bf in (oref pm/config -buffers)
        when (and (buffer-live-p bf)
                  (eq mode (buffer-local-value 'polymode-major-mode bf)))
        return bf))

;; ;; This doesn't work in 24.2, pcase bug ((void-variable xcar))
;; ;; Other pcases in this file don't throw this error
;; (defun pm--set-submode-buffer (obj type buff)
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

;; this is a literal transcript of the pcase above
(defun pm--set-submode-buffer (obj type buff)
  (with-slots (buffer head-mode head-buffer tail-mode tail-buffer) obj
    (cond
     ((and (eq type 'body)
           (eq head-mode 'body)
           (or (null tail-mode)
               (eq tail-mode 'body)))
      (setq buffer buff
            head-buffer buff
            tail-buffer buff))
     ((and (eq type 'body)
           (eq tail-mode 'body))
       (setq buffer buff
             tail-buffer buff))
     ((eq type 'body)
      (setq buffer buff))
     ((and (eq type 'head)
           (or (null tail-mode)
               (eq tail-mode 'head)))
      (setq head-buffer buff
            tail-buffer buff))
     ((eq type 'head)
      (setq head-buffer buff))
     ((and (eq type 'tail)
           (or (null tail-mode)
               (eq tail-mode 'head)))
      (setq tail-buffer buff
            head-buffer buff))
     ((eq type 'tail)
      (setq tail-buffer buff))
     (t (error "type must be one of 'body 'head and 'tail")))))

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
           (oref (oref pm/config -basemode) :mode))
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
  "Abreviate names in list by replacing abrev-regexp with empty string."
  (mapcar (lambda (nm)
            (let ((str-nm (if (symbolp nm)
                              (symbol-name nm)
                            nm)))
              (propertize (replace-regexp-in-string abrev-regexp "" str-nm)
                          :orig str-nm)))
          list))

(defun pm--put-hist (key val)
  (oset pm/config -hist
        (plist-put (oref pm/config -hist) key val)))

(defun pm--get-hist (key)
  (plist-get (oref pm/config -hist) key))

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
    (pop-to-buffer buffer)))

(defun pm--run-command-sentinel (process name display? message)
  (let ((buff (process-buffer process)))
    (with-current-buffer buff
      (goto-char (point-min))
      (let ((case-fold-search t)
            (ofile pm--output-file))
        (if (not (re-search-forward "error" nil 'no-error))
            (progn
              (pop-to-buffer pm--input-buffer)
              (when display?
                (display-buffer (find-file-noselect ofile 'nowarn)))
              buff)
          (display-buffer (current-buffer))
          (error "Bumps while %s: %s" message name))))))


;;; DEFINE
(defmacro define-polymode (mode config &optional keymap &rest body)
  "Define a new polymode MODE.
This macro defines command MODE and an indicator variable MODE
that is t when MODE is active and nil othervise.

MODE command is similar to standard emacs major modes and it can
be used in `auto-mode-alist'. Standard hook MODE-hook is run at
the end of the initialization of each polymode buffer, indirect
and base alike. Additionally MODE-map is created based on the
CONFIG's :map slot and the value of the :keymap argument; see
below.

CONFIG is a name of a config object representing the mode.

MODE command can also be use as a minor mode. Current major mode
is not reinitialized if it coincides with the :mode slot of
CONFIG object or if the :mode slot is nil.

BODY contains code to be executed after the complete
  initialization of the polymode (`pm/initialize') and before
  running MODE-hook. Before the actual body code, you can write
  keyword arguments, i.e. alternating keywords and values.  The
  following special keywords are supported:

:lighter SPEC   Optional LIGHTER is displayed in the mode line when
                the mode is on. If omitted, it defaults to
                the :lighter slot of CONFIG object.
:keymap MAP	Same as the KEYMAP argument.

                If nil, a new MODE-map keymap is created what
                directly inherits from the keymap defined by
                the :map slot of CONFIG object. In most cases it
                is a simple map inheriting form
                `polymode-mode-map'. If t or an alist (of
                bindings suitable to be passed to
                `easy-mmode-define-keymap') a keymap MODE-MAP is
                build by mergin this alist with the :map
                specification of the CONFIG object. If a symbol,
                it should be a variable whose value is a
                keymap. No MODE-MAP is automatically created in
                the latter case and :map slot of the CONFIG
                object is ignored.

:after-hook     A single lisp form which is evaluated after the mode hooks
                have been run.  It should not be quoted."
  (declare 
   (debug (&define name name
                   [&optional [&not keywordp] sexp]
                   [&rest [keywordp sexp]]
                   def-body)))

  (when (keywordp keymap)
    (push keymap body) (setq keymap nil))

  (let* ((last-message (make-symbol "last-message"))
         (mode-name (symbol-name mode))
         (pretty-name (concat
                       (replace-regexp-in-string "poly-\\|-mode" "" mode-name)
                       " polymode"))
	 (group nil)
         (lighter (oref (symbol-value config) :lighter))
	 (extra-keywords nil)
         (modefun mode)          ;The minor mode function name we're defining.
	 (after-hook nil)
	 (hook (intern (concat mode-name "-hook")))
	 keyw keymap-sym key-alist tmp)

    ;; Check keys.
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
	(`:lighter (setq lighter (purecopy (pop body))))
	;; (`:group (setq group (nconc group (list :group (pop body)))))
	(`:keymap (setq keymap (pop body)))
	(`:after-hook (setq after-hook (pop body)))
	(_ (push keyw extra-keywords) (push (pop body) extra-keywords))))

    ;; (unless group
    ;;   ;; We might as well provide a best-guess default group.
    ;;   (setq group
    ;;         `(:group ',(intern (replace-regexp-in-string
    ;;     			"-mode\\'" "" mode-name)))))
    (unless (keymapp keymap)
      ;; keymap is either nil or list
      (setq key-alist keymap)
      (let* ((pi (symbol-value config))
             map mm-name)
        (while pi
          (setq map (and (slot-boundp pi :map)
                         (oref pi :map)))
          (if (and (symbolp map)
                   (keymapp (symbol-value map)))
              (setq keymap  (symbol-value map)
                    pi nil)
            ;; go down to next parent
            (setq pi (and (slot-boundp pi :parent-instance)
                          (oref pi :parent-instance))
                  key-alist (append key-alist map))))))

    (unless keymap
      (setq keymap polymode-mode-map))
    (setq keymap-sym (intern (concat mode-name "-map")))

    `(progn
       ;; Define the variable to enable or disable the mode.
       :autoload-end
       (defvar ,mode nil ,(format "Non-nil if %s is enabled." pretty-name))
       (make-variable-buffer-local ',mode)

       ;; The actual function:
       (defun ,mode (&optional arg) ,(format "%s\n\n\\{%s}"
                                             (concat pretty-name ".")
                                             (or keymap-sym
                                                 (and (null keymap)
                                                      'polymode-mode-map)
                                                 (and (symbolp keymap)
                                                      keymap)))
	 (interactive)
         (unless ,mode
           (let ((,last-message (current-message)))
             (unless pm/config ;; don't reinstall for time being
               (let ((config (clone ,config)))
                 (oset config :minor-mode ',mode)
                 (pm/initialize config)))
             ;; set our "minor" mode
             (setq ,mode t)
             ,@body
             (run-hooks ',hook)
             ;; Avoid overwriting a message shown by the body,
             ;; but do overwrite previous messages.
             (when (and (called-interactively-p 'any)
                        (or (null (current-message))
                            (not (equal ,last-message
                                        (current-message)))))
               (message ,(format "%s enabled" pretty-name)))
             ,@(when after-hook `(,after-hook))
             (force-mode-line-update)))
         ;; Return the new setting.
         ,mode)

       ;;  autoloads everything up-to-here.
       :autoload-end
       
       ;; Define the minor-mode keymap.
       (defvar ,keymap-sym
         (easy-mmode-define-keymap ',key-alist nil nil '(:inherit ,keymap))
         ,(format "Keymap for %s." pretty-name))
       
       (add-minor-mode ',mode ',lighter ,(or keymap-sym keymap)))))


(define-minor-mode polymode-minor-mode
  "Polymode minor mode, used to make everything work."
  nil " PM" polymode-mode-map)



;;; COMPATIBILITY
(defun pm--flyspel-dont-highlight-in-submodes (beg end poss)
  (or (get-text-property beg 'chunkmode)
      (get-text-property beg 'chunkmode)))


;;; FONT-LOCK
;; indulge elisp font-lock :) 
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\(define-polymode\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))

(setq pm--dbg-mode-line t
      pm--dbg-fontlock t
      pm--dbg-hook t)

(provide 'polymode)
