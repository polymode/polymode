(require 'polymode-common)
(require 'poly-lock)


(defgeneric pm-initialize (config)
  "Initialize current buffer with CONFIG.

First initialise the -hostmode and -innermodes slots of polymode
object ...")

(defmethod pm-initialize ((config pm-polymode))
  ;; fixme: reinstalation leads to infloop of pm--fontify-region-original and others ...
  ;; On startup with local auto vars emacs reinstals the mode twice .. waf?
  ;; Temporary fix: don't install twice
  (unless pm/polymode
    (let* ((chunkmode (clone (symbol-value (oref config :hostmode))))
           ;; Set if nil! This allows unspecified host chunkmodes to be used in
           ;; minor modes.
           (host-mode (or (oref chunkmode :mode)
                          (oset chunkmode :mode major-mode))))

      (pm--mode-setup host-mode)
	  	  
	  ;; maybe: fixme: inconsistencies?
      ;; 
	  ;; 1) Not calling pm-install-buffer on host-buffer. But, we are not
	  ;; creating/installing a new buffer here, so it is a different thing and
	  ;; is probably ok.
      ;;
      ;; 2) Not calling config's :minor-mode (polymode function). But polymode
	  ;; function calls pm-initialize, so it's probably ok.
	  (oset chunkmode -buffer (current-buffer))
	  (oset config -hostmode chunkmode)

	  (setq pm/polymode config
			pm/chunkmode chunkmode
			pm/type 'host)

	  (pm--common-setup)
	  
      (add-hook 'flyspell-incorrect-hook 'pm--flyspel-dont-highlight-in-chunkmodes nil t)
	  (pm--run-init-hooks config))))

(defmethod pm-initialize ((config pm-polymode-one))
  (call-next-method)
  (eval `(oset config -innermodes
               (list (clone ,(oref config :innermode))))))

(defmethod pm-initialize ((config pm-polymode-multi))
  (call-next-method)
  (oset config -innermodes
        (mapcar (lambda (sub-name)
                  (clone (symbol-value sub-name)))
                (oref config :innermodes))))

(defun pm--mode-setup (mode &optional buffer)
  ;; General major-mode install. Should work for both indirect and base buffers.
  ;; PM objects are not yet initialized (pm/polymode, pm/chunkmode, pm/type)

  (with-current-buffer (or buffer (current-buffer))
    ;; don't re-install if already there; polymodes can be used as minor modes.
    (unless (eq major-mode mode)
     (let ((polymode-mode t) ;major-modes might check this
           (font-lock-fontified t)
           ;; Modes often call font-lock functions directly. We prevent that.
           (font-lock-function 'ignore)
           (font-lock-flush-function 'ignore)
           (font-lock-fontify-buffer-function 'ignore)
           ;; Mode functions can do arbitrary things. We inhibt all PM hooks
           ;; because PM objects have not been setup yet.
           (pm-debug-allow-post-command-hook nil)
           (poly-lock-allow-after-change nil)
           (poly-lock-allow-fontification nil))
       (condition-case-unless-debug err
           (funcall mode)
         (error (message "Polymode error (pm--mode-setup '%s): %s" mode (error-message-string err))))))

	(setq polymode-mode t)

	(current-buffer)))

(defun pm-syntax-begin-function ()
  (goto-char
   (max (car (pm-get-innermost-range))
        (if pm--syntax-begin-function-original
            (save-excursion
              (funcall pm--syntax-begin-function-original)
              (point))
          (point-min)))))

(defun pm--common-setup (&optional buffer)
  ;; General buffer setup. Should work for indirect and base buffers. Assumes
  ;; that the buffer was fully prepared and objects like pm/polymode and
  ;; pm/chunkmode have been initialised. Return the BUFFER.

  (with-current-buffer (or buffer (current-buffer))

	(add-hook 'kill-buffer-hook 'pm--kill-indirect-buffer t t)
    
	;; (setq-local font-lock-mode t)
	(setq-local font-lock-function 'poly-lock-mode)
	(setq-local font-lock-support-mode 'poly-lock-mode)
	(setq-local pm--fontify-region-original font-lock-fontify-region-function)
	(setq-local font-lock-fontify-region-function #'poly-lock-fontify-region)
    (font-lock-mode t)

    (setq pm--syntax-begin-function-original syntax-begin-function)
	(setq-local syntax-begin-function #'pm-syntax-begin-function)

	(when (and indent-line-function ; not that it should ever be nil...
			   (oref pm/chunkmode :protect-indent-line))
	  (setq pm--indent-line-function-original indent-line-function)
	  (set (make-local-variable 'indent-line-function) 'pm-indent-line-dispatcher))
	
	(add-hook 'post-command-hook 'polymode-post-command-select-buffer nil t)
	(object-add-to-list pm/polymode '-buffers (current-buffer))
	(current-buffer)))

(defun pm--run-init-hooks (config)
  "Run hooks from :init-functions slot of CONFIG and its parent instances.
Parents' hooks are run first."
  (let ((parent-inst config) 
		init-funs)
	;; run hooks, parents first
	(while parent-inst
	  (setq init-funs (append (and (slot-boundp parent-inst :init-functions) ; don't cascade
							 (oref parent-inst :init-functions))
						init-funs)
			parent-inst (and (slot-boundp parent-inst :parent-instance)
					(oref parent-inst :parent-instance))))
	(run-hooks 'init-funs)))


(defgeneric pm-install-buffer (chunkmode &optional type)
  "Get or create an indirect buffer and install it into the relevant slot(s) of CHUNKMODE.
Should return newly installed buffer.")

(defmethod pm-install-buffer ((chunkmode pm-chunkmode) &optional type)
  "Retrieve or create+initialize if doesn't exist the indirect buffer for CHUNKMODE.
The buffer is assigned to CHUNKMODE's -buffer slot."
  (oset chunkmode -buffer
        (pm--get-chunkmode-buffer-create chunkmode type)))

(defmethod pm-install-buffer ((chunkmode pm-hbtchunkmode) type)
  "Retrieve or create+initialize if doesn't exist the indirect buffer for CHUNKMODE.
The buffer is assigned to one or more CHUNKMODE's slots
`-buffer', `-head-buffer' or `-tail-buffer'. This assignment
depends on the TYPE and the values of `tail-mode' and `head-mode'
slots of the CHUNKMODE object. See `pm--set-chunkmode-buffer' for
how this is computed."
  (pm--set-chunkmode-buffer chunkmode type
							(pm--get-chunkmode-buffer-create chunkmode type)))

;; This doesn't work in 24.2, pcase bug ((void-variable xcar)) Other pcases in
;; this file don't throw this error.
(defun pm--set-chunkmode-buffer (obj type buff)
  "Assign BUFF to OBJ's slot(s) corresponding to TYPE."
  (with-slots (-buffer head-mode -head-buffer tail-mode -tail-buffer) obj
    (pcase (list type head-mode tail-mode)
      (`(body body ,(or `nil `body))
       (setq -buffer buff
             -head-buffer buff
             -tail-buffer buff))
      (`(body ,_ body)
       (setq -buffer buff
             -tail-buffer buff))
      (`(body ,_ ,_ )
       (setq -buffer buff))
      (`(head ,_ ,(or `nil `head))
       (setq -head-buffer buff
             -tail-buffer buff))
      (`(head ,_ ,_)
       (setq -head-buffer buff))
      (`(tail ,_ ,(or `nil `head))
       (setq -tail-buffer buff
             -head-buffer buff))
      (`(tail ,_ ,_)
       (setq -tail-buffer buff))
      (_ (error "type must be one of 'body, 'head or 'tail")))))

(defun pm--get-chunkmode-buffer-create (chunkmode type)
  ;; assumes pm/polymode is set
  (let ((mode (pm--get-chunkmode-mode chunkmode type)))
    (or (pm--get-indirect-buffer-of-mode mode)
		(pm--create-indirect-buffer chunkmode type mode))))

(defun pm--get-indirect-buffer-of-mode (mode)
  (loop for bf in (oref pm/polymode -buffers)
        when (and (buffer-live-p bf)
                  (eq mode (buffer-local-value 'major-mode bf)))
        return bf))

(defvar pm--ib-prefix "")
(defun pm--create-indirect-buffer (chunkmode type &optional mode)

  (let ((config (or (buffer-local-value 'pm/polymode (pm-base-buffer))
                    (error "`pm/polymode' not found in the base buffer %s"
                           (pm-base-buffer)))))

   (setq mode (pm--get-available-mode
               (or mode (pm--get-chunkmode-mode chunkmode type))))

   (with-current-buffer (pm-base-buffer)
     (let* ((new-name
             (generate-new-buffer-name
              (format "%s%s[%s]" pm--ib-prefix (buffer-name)
                      (replace-regexp-in-string "-mode" "" (symbol-name mode)))))
            (new-buffer (make-indirect-buffer (current-buffer)  new-name))
            (file (buffer-file-name))
            (base-name (buffer-name))
            (coding buffer-file-coding-system))

       (with-current-buffer new-buffer
         (pm--mode-setup mode)

         (setq buffer-file-coding-system coding
               ;; Avoid the uniqified name for the indirect buffer in the mode line
               ;; mode-line-buffer-identification (propertized-buffer-identification base-name)
               pm/polymode config
               pm/chunkmode chunkmode
               pm/type type)
         (funcall (oref pm/polymode :minor-mode))
         (vc-find-file-hook)

         (pm--common-setup))
       
       new-buffer))))


(defgeneric pm-get-buffer (chunkmode &optional span-type)
  "Get the indirect buffer associated with SUBMODE and
SPAN-TYPE. Should return nil if buffer has not yet been
installed. Also see `pm-get-span'.")

(defmethod pm-get-buffer ((chunkmode pm-chunkmode) &optional type)
  (oref chunkmode -buffer))

(defmethod pm-get-buffer ((chunkmode pm-hbtchunkmode) &optional type)
  (cond ((eq 'body type) (oref chunkmode -buffer))
        ((eq 'head type) (oref chunkmode -head-buffer))
        ((eq 'tail type) (oref chunkmode -tail-buffer))
        (t (error "Don't know how to select buffer of type '%s' for chunkmode '%s' of class '%s'"
                  type (pm--object-name chunkmode) (class-of chunkmode)))))


(defgeneric pm-select-buffer (chunkmode span)
  "Ask SUBMODE to select (make current) its indirect buffer
corresponding to the type of the SPAN returned by
`pm-get-span'.")

(defmethod pm-select-buffer ((chunkmode pm-chunkmode) span)
  "Select the buffer associated with CHUNKMODE.
Install a new indirect buffer if it is not already installed. For
this method to work correctly, SUBMODE's class should define
`pm-install-buffer' and `pm-get-buffer' methods."
  (let* ((type (car span))
		 (buff (pm-get-buffer chunkmode type)))
	(unless (buffer-live-p buff)
	  (pm-install-buffer chunkmode type)
	  (setq buff (pm-get-buffer chunkmode type)))
	(pm--select-existent-buffer buff)))

(defvar pm--select-buffer-visually)
;; extracted for debugging and tracing
(defun pm--select-existent-buffer (buffer)
  (when (and (not (eq buffer (current-buffer)))
			 (buffer-live-p buffer))
	(if (or (not (boundp 'pm--select-buffer-visually))
            (not pm--select-buffer-visually))
		;; fast selection
		(set-buffer buffer)
	  ;; slow, visual selection
	  (pm--select-existent-buffer-visually buffer))))

;; extracted for debugging and tracing
(defun pm--select-existent-buffer-visually (buffer)
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
    ;; avoid display jumping
    (when visible
      (set-window-start (get-buffer-window buffer t) window-start))))

(defmethod pm-select-buffer ((chunkmode pm-hbtchunkmode) span)
  (call-next-method)
  (pm--transfer-vars-from-base))

(defmethod pm-select-buffer ((config pm-polymode-multi-auto) &optional span)
  ;; :fixme: pm-get-span on multi configs returns config as last object of
  ;; span. That's freaking confusing.
  (if (null (car span))
      (pm-select-buffer (oref config -hostmode) span)
    (let ((type (car span))
          (proto (symbol-value (oref config :auto-innermode)))
          chunkmode)
      (save-excursion
        (goto-char (cadr span))
        (unless (eq type 'head)
          (re-search-backward (oref proto :head-reg) nil 'noerr))
        (let* ((str (or (and (oref proto :retriever-regexp)
                             (re-search-forward (oref proto :retriever-regexp))
                             (match-string-no-properties (oref proto :retriever-num)))
                        (and (oref proto :retriever-function)
                             (funcall (oref proto :retriever-function)))
                        (error "retriever subexpression didn't match")))
               (name (concat "auto-innermode:" str)))
          (setq chunkmode
                (or (loop for obj in (oref config -auto-innermodes)
                          when  (equal name (object-name-string obj))
                          return obj)
                    (let ((new-obj (clone proto name
                                          :mode (pm--get-mode-symbol-from-name str))))
                      (object-add-to-list config '-auto-innermodes new-obj)
                      new-obj)))))
      (pm-select-buffer chunkmode span))))


(defun pm--adjust-visual-line-mode (vlm)
  (unless (eq visual-line-mode vlm)
    (if (null vlm)
        (visual-line-mode -1)
      (visual-line-mode 1))))

(defun pm--move-overlays-to (new-buff)
  (mapc (lambda (o)
          (move-overlay o (overlay-start o) (overlay-end o) new-buff))
        (overlays-in 1 (1+ (buffer-size)))))

(defun pm--transfer-vars-from-base ()
  (let ((bb (pm-base-buffer)))
    (dolist (var '(buffer-file-name))
      (set var (buffer-local-value var bb)))))

(defvar-local pm--killed-once nil)
(defun pm--kill-indirect-buffer ()
  ;; find-alternate-file breaks (https://github.com/vspinu/polymode/issues/79)
  (let ((base (buffer-base-buffer)))
	(when  (and base (buffer-live-p base))
	  ;; 'base' is non-nil in indirect buffers only
	  (set-buffer-modified-p nil)
	  (unless (buffer-local-value 'pm--killed-once base)
		(with-current-buffer base
		  (setq pm--killed-once t))
		(kill-buffer base)))))

(defun pm--get-chunkmode-mode (obj type)
  (with-slots (mode head-mode tail-mode) obj
    (cond ((or (eq type 'body)
               (and (eq type 'head)
                    (eq head-mode 'body))
               (and (eq type 'tail)
                    (or (eq tail-mode 'body)
                        (and (or (null tail-mode)
								 (eq tail-mode 'head))
                             (eq head-mode 'body)))))
           (oref obj :mode))
          ((or (and (eq type 'head)
                    (eq head-mode 'host))
               (and (eq type 'tail)
                    (or (eq tail-mode 'host)
                        (and (or (null tail-mode)
								 (eq tail-mode 'head))
                             (eq head-mode 'host)))))
           (oref (oref pm/polymode -hostmode) :mode))
          ((eq type 'head)
           (oref obj :head-mode))
          ((eq type 'tail)
		   (if (or (null tail-mode)
				   (eq tail-mode 'head))
			   (oref obj :head-mode)
			 (oref obj :tail-mode)))
          (t (error "type must be one of 'head 'tail 'body")))))


;;; SPAN MANIPULATION

(defgeneric pm-get-span (chunkmode &optional pos)
  "Ask the CHUNKMODE for the span at point.
Return a list of three elements (TYPE BEG END OBJECT) where TYPE
is a symbol representing the type of the span surrounding
POS (head, tail, body). BEG and END are the coordinates of the
span. OBJECT is a sutable object which is 'responsable' for this
span. This is an object that could be dispached upon with
`pm-select-buffer',  .. (fixme: complete this list).

Should return nil if there is no SUBMODE specific span around POS.")

(defmethod pm-get-span (chunkmode &optional pos)
  "Return nil.
Base mode usually do not compute the span."
  (unless chunkmode
	(error "Dispatching `pm-get-span' on a nil object"))
  nil)

(defmethod pm-get-span ((config pm-polymode) &optional pos)
  "Apply pm-get-span on every element of chunkmodes slot of config object.
Return a cons (chunkmode . span), for which START is closest to
POS (and before it); i.e. the innermost span.  POS defaults to
point."
  (save-restriction
	(let (pm--restrict-widen)
	  (widen)
	  ;; fixme: host should be last, to take advantage of the chunkmodes computation
	  (let* ((smodes (cons (oref config -hostmode)
						   (oref config -innermodes)))
			 (start (point-min))
			 (end (point-max))
			 (pos (or pos (point)))
			 (span (list nil start end nil))
			 val)

		(dolist (sm smodes)
		  (setq val (pm-get-span sm pos))
		  (when (and val
					 (or (> (nth 1 val) start)
						 (< (nth 2 val) end)))
			(if (or (car val)
					(null span))
				(setq span val
					  start (nth 1 val)
					  end (nth 2 val))
			  ;; nil car means outer chunkmode (usually host). And it can be an
			  ;; intersection of spans returned by 2 different neighbour inner
			  ;; chunkmodes. See rapport mode for an example
			  (setq start (max (nth 1 val)
							   (nth 1 span))
					end (min (nth 2 val)
							 (nth 2 span)))
			  (setcar (cdr span) start)
			  (setcar (cddr span) end))))

		(unless (and (<= start end) (<= pos end) (>= pos start))
		  (error "Bad polymode selection: span:%s pos:%s"
				 (list start end) pos))
		(when (null (car span)) ; chunkmodes can compute the host span by returning nil
		  (setcar (last span) (oref config -hostmode)))
		span))))

;; No need for this one so far. Basic method iterates through -innermodes
;; anyhow.
;; (defmethod pm-get-span ((config pm-polymode-multi) &optional pos))

(defmethod pm-get-span ((config pm-polymode-multi-auto) &optional pos)
  (let ((span-other (call-next-method))
        (proto (symbol-value (oref config :auto-innermode))))
    (if (oref proto :head-reg)
        (let ((span (pm--span-at-point (oref proto :head-reg)
                                       (oref proto :tail-reg)
                                       pos)))
          (if (and span-other
                   (or (> (nth 1 span-other) (nth 1 span))
                       (< (nth 2 span-other) (nth 2 span))))
              ;; treat intersections with the host mode
              (if (car span-other)
                  span-other ;not host
                ;; here, car span should better be nil; no explicit check
                (setcar (cdr span-other) (max (nth 1 span-other) (nth 1 span)))
                (setcar (cddr span-other) (min (nth 2 span-other) (nth 2 span)))
                span-other)
            (append span (list config)))) ;fixme: this returns config as last object
      span-other)))

(defmethod pm-get-span ((chunkmode pm-hbtchunkmode) &optional pos)
  "Return a list of the form (TYPE POS-START POS-END SELF).
TYPE can be 'body, 'head or 'tail. SELF is just a chunkmode object
in this case."
  (with-slots (head-reg tail-reg head-mode tail-mode) chunkmode
    (let* ((span (pm--span-at-point head-reg tail-reg pos))
           (type (car span)))
      (when (or (and (eq type 'head) (eq head-mode 'host))
                (and (eq type 'tail) (or (eq tail-mode 'host)
                                         (and (null tail-mode)
                                              (eq head-mode 'host)))))
        (setcar span nil))
      (append span (list chunkmode)))))

(defmacro pm-create-indented-block-matchers (name regex)
  "Defines 2 functions, each return a list of the start and end points of the
HEAD and TAIL portions of an indented block of interest, via some regex.
You can then use these functions in the defcustom pm-inner modes.

e.g.
(pm-create-indented-block-matchers 'slim-coffee' \"^[^ ]*\\(.*:? *coffee: *\\)$\")

creates the functions

pm-slim-coffee-head-matcher
pm-slim-coffee-tail-matcher

In the example below,

The head matcher will match against 'coffee:', returning the positions of the
start and end of 'coffee:'
The tail matcher will return a list (n, n) of the final characters is the block.

    |<----- Uses this indentation to define the left edge of the 'block'
    |
    |<--->|  This region is higlighted by the :head-mode in the block-matchers
    |     |
    |     |<----- the head matcher uses this column as the end of the head
    |     |
----:-----:-------------- example file -----------------------------------------
1|  :     :
2|  coffee:
3|    myCoffeeCode()
4|    moreCode ->
5|      do things
6|              :
7|  This is no longer in the block
8|              :
----------------:---------------------------------------------------------------
            --->|<----- this region of 0 width is highlighted by the :tail-mode
                        the 'block' ends after this column on line 5


All the stuff after the -end- of the head and before the start of the tail is
sent to the new mode for syntax highlighting."
  (let* ((head-name (intern (format "pm-%s-head-matcher" name)))
         (tail-name (intern (format "pm-%s-tail-matcher" name))))
    `(progn
       (defun ,head-name (ahead)
         (when (re-search-forward ,regex nil t ahead)
           (cons (match-beginning 1) (match-end 1))))

       (defun ,tail-name (ahead)
         (save-excursion
           ;; (cons (point-max) (point-max)))))))
           (goto-char (car (,head-name 1)))
           (let* ((block-col (current-indentation))
                  (posn (catch 'break
                          (while (not (eobp))
                            (forward-line 1)
                            (when (and (<= (current-indentation) block-col)
                                       (not (progn
                                              (beginning-of-line)
                                              (looking-at "^[[:space:]]*$"))))
                              (throw 'break (point-at-bol))))
                          (throw 'break (point-max)))))
             (cons posn posn)))))))

(defun pm--default-matcher (reg ahead)
  (if (< ahead 0)
      (if (re-search-backward reg nil t)
          (cons (match-beginning 0) (match-end 0)))
    (if (re-search-forward reg nil t)
        (cons (match-beginning 0) (match-end 0)))))

;; fixme: there should be a simpler way... check the code and document
(defun pm--span-at-point-fun-fun (hd-matcher tl-matcher)
  (save-excursion
    (let ((pos (point))
          (posh (funcall hd-matcher -1)))
      (if (null posh)
          ;; special first chunk
          (let ((posh1 (progn (goto-char (point-min))
                              (funcall hd-matcher 1))))
            (if (and posh1
                     (<= (car posh1) pos)
                     (< pos (cdr posh1)))
                (list 'head (car posh1) (cdr posh1))
              (list nil (point-min) (or (car posh1)
                                        (point-max)))))
        (let ((post (progn (goto-char (car posh))
                           (or (funcall tl-matcher 1)
                               (cons (point-max) (point-max))))))
          (if (and (<= (cdr posh) pos)
                   (< pos (car post)))
              (list 'body (cdr posh) (car post))
            (if (and (<= (car post) pos)
                     (< pos (cdr post)))
                (list 'tail (car post) (cdr post))
              (if (< pos (cdr post))
                  ;; might be in the head
                  (progn
                    (goto-char (car post))
                    (let ((posh1 (funcall hd-matcher -1)))
                      (if (and (<= (car posh1) pos)
                               (< pos (cdr posh1)))
                          (list 'head (car posh1) (cdr posh1))
                        (list nil (cdr posh) (car posh1))))) ;; posh is point min, fixme: not true anymore?
                (goto-char (cdr post))
                (let ((posh1 (or (funcall hd-matcher 1)
                                 (cons (point-max) (point-max)))))
                  (if (and posh
                           (<= (car posh1) pos )
                           (< pos (cdr posh1)))
                      (list 'head (car posh1) (cdr posh1))
                    (list nil (cdr post) (car posh1))))))))))))

(defun pm--span-at-point-reg-reg (head-matcher tail-matcher)
  ;; Guaranteed to produce non-0 length spans. If no span has been found
  ;; (head-matcher didn't match) return (nil (point-min) (point-max)).

  ;; xxx1 relate to the first ascending search
  ;; xxx2 relate to the second descending search
  (save-excursion
    (let* ((pos (point))
		   
		   (head1-beg (and (re-search-backward head-matcher nil t)
						   (match-beginning 0)))
		   (head1-end (and head1-beg (match-end 0))))
	  
	  (if head1-end
		  ;; we know that (>= pos head1-end)
		  ;;            -----------------------
		  ;; host](head)[body](tail)[host](head)
		  (let* ((tail1-beg (and (goto-char head1-end)
								 (re-search-forward tail-matcher nil t)
								 (match-beginning 0)))
				 (tail1-end (and tail1-beg (match-end 0)))
				 (tail1-beg (or tail1-beg (point-max)))
				 (tail1-end (or tail1-end (point-max))))

			(if (or (< pos tail1-end)
					(= tail1-end (point-max)))
				(if (<= pos tail1-beg)
					;;            ------
					;; host](head)[body](tail)[host](head))
					(list 'body head1-end tail1-beg)
				  ;;                  -----
				  ;; host](head](body](tail)[host](head)
				  (list 'tail tail1-beg tail1-end))
			  
			  ;;                        ------------
			  ;; host](head](body](tail)[host](head)
			  (let* ((head2-beg (or (and (re-search-forward head-matcher nil t)
										 (match-beginning 0))
									(point-max))))
				(if (<= pos head2-beg)
					;;                        ------
					;; host](head](body](tail)[host](head)
					(list nil tail1-end head2-beg)
				  ;;                              ------
				  ;; host](head](body](tail)[host](head)
				  (list 'head head2-beg (match-end 0))))))

		;; -----------
		;; host](head)[body](tail)[host
		(let ((head2-beg (and (goto-char (point-min))
							  (re-search-forward head-matcher nil t)
							  (match-beginning 0))))

		  (if (null head2-beg)
			  ;; no span found
			  (list nil (point-min) (point-max))
			
			(if (<= pos head2-beg)
				;; -----
				;; host](head)[body](tail)[host
				(list nil (point-min) head2-beg)
			  ;;      ------
			  ;; host](head)[body](tail)[host
			  (list 'head head2-beg (match-end 0)))))))))

(defun pm--span-at-point (head-matcher tail-matcher &optional pos)
  "Basic span detector with head/tail.

Either of HEAD-MATCHER and TAIL-MATCHER can be a regexp or a
function. When a function the matcher must accept one argument
that can take either values 1 (forwards search) or -1 (backward
search). This function must return either nil (no match) or
a (cons BEG END) representing the span of the head or tail
respectively. See `pm--default-matcher' for an example.

Return (type span-start span-end) where type is one of the
follwoing symbols:

nil	  - pos is between point-min and head-reg, or between tail-reg and point-max
body  - pos is between head-reg and tail-reg (exclusively)
head  - head span
tail  - tail span"
  ;; ! start of the span is part of the span !
  (save-restriction
	(let (pm--restrict-widen)
	  (widen)
	  (goto-char (or pos (point)))
	  (cond ((and (stringp head-matcher)
				  (stringp tail-matcher))
			 (pm--span-at-point-reg-reg head-matcher tail-matcher))
			((and (stringp head-matcher)
				  (functionp tail-matcher))
			 (pm--span-at-point-fun-fun
			  (lambda (ahead) (pm--default-matcher head-matcher ahead))
			  tail-matcher))
			((and (functionp head-matcher)
				  (stringp tail-matcher))
			 (pm--span-at-point-fun-fun
			  head-matcher
			  (lambda (ahead) (pm--default-matcher tail-matcher ahead))))
			((and (functionp head-matcher)
				  (functionp tail-matcher))
			 (pm--span-at-point-fun-fun head-matcher tail-matcher))
			(t (error "head and tail matchers should be either regexp strings or functions"))))))


;;; INDENT
(defun pm-indent-line-dispatcher ()
  "Dispatch methods indent methods on current span."
  (let ((span (pm-get-innermost-span)))
    (pm-indent-line (car (last span)) span)))

(defgeneric pm-indent-line (&optional chunkmode span)
  "Indent current line.
Protect and call original indentation function associated with
the chunkmode.")

(defun pm--indent-line (span)
  ;; istr is auto-indent string
  (unwind-protect
      (save-restriction
        (pm--comment-region  1 (nth 1 span))
        (pm-narrow-to-span span)
        (funcall pm--indent-line-function-original))
    (pm--uncomment-region 1 (nth 1 span))))

(defmethod pm-indent-line ((chunkmode pm-chunkmode) &optional span)
  (pm--indent-line span))

(defmethod pm-indent-line ((chunkmode pm-hbtchunkmode) &optional span)
  "Indent line in inner chunkmodes.
When point is at the beginning of head or tail, use parent chunk
to indent."
  ;; sloppy work:
  ;; Assumes multiline chunks and single-line head/tail.
  ;; Assumes current buffer is the correct buffer.
  (let ((pos (point))
        shift delta)
    (cond ((or (eq 'head (car span))
               (eq 'tail (car span)))
           ;; use parent's indentation function in head and tail
           (back-to-indentation)
           (setq delta (- pos (point)))
           (backward-char)
           (let ((parent-span (pm-get-innermost-span)))
             (pm-select-buffer (car (last parent-span)) parent-span)
             (forward-char)
             (pm--indent-line parent-span)
             (when (eq 'tail (car span))
               (setq shift (pm--get-head-shift parent-span))
               (indent-to (+ shift (- (point) (point-at-bol))))))
           (if (> delta 0)
               (goto-char (+ (point) delta))))
          (t
           (setq shift (pm--get-head-shift span))
           (pm--indent-line span)
           (when (= (current-column) 0)
             (setq shift (+ shift (oref chunkmode :indent-offset))))
           (setq delta (- (point) (point-at-bol)))
           (beginning-of-line)
           (indent-to shift)
           (goto-char (+ (point) delta))))))

;; fixme: This one is nowhere used?
(defmethod pm-indent-line ((chunkmode pm-polymode-multi-auto) &optional span)
  (pm-select-buffer chunkmode span)
  (pm-indent-line pm/chunkmode span))

(defun pm--get-head-shift (span)
  (save-excursion
    (goto-char (cadr span))
    (back-to-indentation)
    (- (point) (point-at-bol))))



;;; FACES
(defgeneric pm-get-adjust-face (chunkmode &optional type))
(defmethod pm-get-adjust-face ((chunkmode pm-chunkmode) &optional type)
  (oref chunkmode :adjust-face))
(defmethod pm-get-adjust-face ((chunkmode pm-hbtchunkmode) &optional type)
  (setq type (or type pm/type))
  (cond ((eq type 'head)
         (oref chunkmode :head-adjust-face))
        ((eq type 'tail)
         (if (eq 'head (oref pm/chunkmode :tail-adjust-face))
             (oref pm/chunkmode :head-adjust-face)
           (oref pm/chunkmode :tail-adjust-face)))
        (t (oref pm/chunkmode :adjust-face))))

(defun pm--get-adjusted-background (prop)
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
                           (list (cons 'background-color
                                       (pm--get-adjusted-background face))))
                      face))
            (pchange nil))
        ;; (while (not (eq pchange end))
        ;;   (setq pchange (next-single-property-change beg 'face nil end))
        ;;   (put-text-property beg pchange 'face
        ;;                      `(,face ,@(get-text-property beg 'face)))
        ;;   (setq beg pchange))
        (font-lock-prepend-text-property beg end 'face face)))))

(provide 'polymode-methods)
