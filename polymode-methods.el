(require 'polymode-core)
(require 'poly-lock)


;;; Initialization

(defgeneric pm-initialize (config)
  "Initialize current buffer with CONFIG.")

(defmethod pm-initialize ((config pm-polymode))
  ;; fixme: (VS[06-03-2016]: probably not anymore) reinstalation leads to
  ;; infloop of poly-lock--fontify-region-original and others ... On startup with local
  ;; auto vars emacs reinstals the mode twice .. waf? Temporary fix: don't
  ;; install twice
  (unless pm/polymode
    (let ((chunkmode (clone (symbol-value (oref config :hostmode)))))
      (let ((pm-initialization-in-progress t)
            ;; Set if nil! This allows unspecified host chunkmodes to be used in
            ;; minor modes.
            (host-mode (or (oref chunkmode :mode)
                           (oset chunkmode :mode major-mode))))

        (pm--mode-setup host-mode)

        ;; maybe: fixme: inconsistencies?
        ;; 1) Not calling config's :minor-mode (polymode function). But polymode
        ;; function calls pm-initialize, so it's probably ok.
        (oset chunkmode -buffer (current-buffer))
        (oset config -hostmode chunkmode)

        (setq pm/polymode config
              pm/chunkmode chunkmode
              pm/type 'host)

        (pm--common-setup)
        (add-hook 'flyspell-incorrect-hook
                  'pm--flyspel-dont-highlight-in-chunkmodes nil t))
      (pm--run-init-hooks config 'polymode-init-host-hook)
      (pm--run-init-hooks chunkmode))))

(defmethod pm-initialize ((config pm-polymode-one))
  (let ((pm-initialization-in-progress t))
    (call-next-method))
  (eval `(oset config -innermodes
               (list (clone ,(oref config :innermode)))))
  (pm--run-init-hooks config 'polymode-init-host-hook))

(defmethod pm-initialize ((config pm-polymode-multi))
  (let ((pm-initialization-in-progress))
    (call-next-method))
  (oset config -innermodes
        (mapcar (lambda (sub-name)
                  (clone (symbol-value sub-name)))
                (oref config :innermodes)))
  (pm--run-init-hooks config 'polymode-init-host-hook))

(defmethod pm-initialize ((chunkmode pm-chunkmode) &optional type mode)
  ;; run in chunkmode indirect buffer
  (setq mode (or mode (pm--get-chunkmode-mode chunkmode type)))
  (let ((pm-initialization-in-progress t)
        (new-name  (generate-new-buffer-name
                    (format "%s[%s]" (buffer-name (pm-base-buffer))
                            (replace-regexp-in-string "-mode" "" (symbol-name mode))))))
    (rename-buffer new-name)
    (pm--mode-setup (pm--get-existent-mode mode))
    (pm--move-vars '(pm/polymode buffer-file-coding-system) (pm-base-buffer))
    (setq pm/chunkmode chunkmode
          pm/type type)
    (funcall (oref pm/polymode :minor-mode))
    (vc-find-file-hook)
    (pm--common-setup)
    (pm--run-init-hooks chunkmode 'polymode-init-inner-hook)))

(defun pm--mode-setup (mode &optional buffer)
  ;; General major-mode install. Should work for both indirect and base buffers.
  ;; PM objects are not yet initialized (pm/polymode, pm/chunkmode, pm/type)

  (with-current-buffer (or buffer (current-buffer))
    ;; don't re-install if already there; polymodes can be used as minor modes.
    (unless (eq major-mode mode)
     (let ((polymode-mode t) ;major-modes might check this
           ;; (font-lock-fontified t)
           ;; Modes often call font-lock functions directly. We prevent that.
           (font-lock-function 'ignore)
           (font-lock-flush-function 'ignore)
           (font-lock-fontify-buffer-function 'ignore)
           ;; Mode functions can do arbitrary things. We inhibt all PM hooks
           ;; because PM objects have not been setup yet.
           (pm-allow-after-change-hook nil)
           (pm-allow-fontification nil))
       (condition-case-unless-debug err
           (funcall mode)
         (error (message "Polymode error (pm--mode-setup '%s): %s" mode (error-message-string err))))))

    (setq polymode-mode t)
    (current-buffer)))

(defun pm--common-setup (&optional buffer)
  ;; General buffer setup. Should work for indirect and base buffers. Assumes
  ;; that the buffer was fully prepared and objects like pm/polymode and
  ;; pm/chunkmode have been initialised. Return the BUFFER.

  (with-current-buffer (or buffer (current-buffer))

    ;; INDENTATION
    (when (and indent-line-function ; not that it should ever be nil...
               (oref pm/chunkmode :protect-indent-line))
      (setq pm--indent-line-function-original indent-line-function)
      (setq-local indent-line-function 'pm-indent-line-dispatcher))

    ;; FONT LOCK
    (setq-local font-lock-function 'poly-lock-mode)
    (font-lock-mode t)

    ;; SYNTAX
    ;; We are executing `syntax-propertize' narrowed to span as per advice in
    ;; (polymode-compat.el)
    (pm-around-advice syntax-begin-function 'pm-override-output-position) ; obsolete as of 25.1
    (pm-around-advice syntax-propertize-extend-region-functions 'pm-override-output-cons)
    ;; flush ppss in all buffers and hook checks
    (add-hook 'before-change-functions 'polymode-before-change-setup t t)

    ;; REST
    (add-hook 'kill-buffer-hook 'pm--kill-indirect-buffer t t)
    (add-hook 'post-command-hook 'polymode-post-command-select-buffer nil t)
    (object-add-to-list pm/polymode '-buffers (current-buffer))

    (current-buffer)))

(defun pm--run-init-hooks (object &optional emacs-hook)
  (unless pm-initialization-in-progress
    (when emacs-hook
      (run-hooks emacs-hook))
    (pm--run-hooks object :init-functions)))

(defun pm--run-hooks (object slot &rest args)
  "Run hooks from SLOT of OBJECT and its parent instances.
Parents' hooks are run first."
  (let ((inst object)
        funs)
    ;; run hooks, parents first
    (while inst
      (setq funs (append (and (slot-boundp inst slot) ; don't cascade
                              (eieio-oref inst slot))
                         funs)
            inst (and (slot-boundp inst :parent-instance)
                      (oref inst :parent-instance))))
    (if args
        (apply 'run-hook-with-args 'funs args)
      (run-hooks 'funs))))

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


(defgeneric pm-get-buffer-create (chunkmode &optional type)
  "Get the indirect buffer associated with SUBMODE and
SPAN-TYPE. Should return nil if buffer has not yet been
installed. Also see `pm-get-span'.")

(defmethod pm-get-buffer-create ((chunkmode pm-chunkmode) &optional type)
  (let ((buff (oref chunkmode -buffer)))
    (or (and (buffer-live-p buff) buff)
        (oset chunkmode -buffer
              (pm--get-chunkmode-buffer-create chunkmode type)))))

(defmethod pm-get-buffer-create ((chunkmode pm-hbtchunkmode) &optional type)
  (let ((buff (cond ((eq 'body type) (oref chunkmode -buffer))
                    ((eq 'head type) (oref chunkmode -head-buffer))
                    ((eq 'tail type) (oref chunkmode -tail-buffer))
                    (t (error "Don't know how to select buffer of type '%s' for chunkmode '%s' of class '%s'"
                              type (eieio-object-name chunkmode) (class-of chunkmode))))))
    (if (buffer-live-p buff)
        buff
      (pm--set-chunkmode-buffer chunkmode type
                                (pm--get-chunkmode-buffer-create chunkmode type)))))

(defun pm--get-chunkmode-buffer-create (chunkmode type)
  (let ((mode (pm--get-existent-mode
               (pm--get-chunkmode-mode chunkmode type))))
    (or
     ;; 1. look through existent buffer list
     (loop for bf in (oref pm/polymode -buffers)
           when (and (buffer-live-p bf)
                     (eq mode (buffer-local-value 'major-mode bf)))
           return bf)
     ;; 2. create new
     (with-current-buffer (pm-base-buffer)
       (let* ((new-name (generate-new-buffer-name (buffer-name)))
              (new-buffer (make-indirect-buffer (current-buffer) new-name)))
         (with-current-buffer new-buffer
           (pm-initialize chunkmode type mode))
         new-buffer)))))

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


(defvar pm-move-vars-from-base '(buffer-file-name)
  "Variables transferred from base buffer on buffer switch.")

(defvar pm-move-vars-from-old-buffer
  '(buffer-invisibility-spec
    selective-display overwrite-mode
    ;; truncation and word-wrap
    truncate-lines word-wrap
    line-move-visual truncate-partial-width-windows)
  "Variables transferred from old buffer on buffer switch.")

(defgeneric pm-select-buffer (chunkmode span)
  "Ask SUBMODE to select (make current) its indirect buffer
corresponding to the type of the SPAN returned by
`pm-get-span'.")

(defmethod pm-select-buffer ((chunkmode pm-chunkmode) span)
  "Select the buffer associated with CHUNKMODE.
Install a new indirect buffer if it is not already installed. For
this method to work correctly, SUBMODE's class should define
`pm-get-buffer-create' methods."
  (let* ((type (car span))
         (buff (pm-get-buffer-create chunkmode type)))
    (pm--select-existent-buffer buff)))

;; extracted for debugging purpose
(defun pm--select-existent-buffer (buffer)
  (when (and (not (eq buffer (current-buffer)))
             (buffer-live-p buffer))
    (pm--move-vars pm-move-vars-from-base (pm-base-buffer) buffer)
    (if pm--select-buffer-visibly
        ;; slow, visual selection
        (pm--select-existent-buffer-visibly buffer)
      ;; fast set-buffer
      (set-buffer buffer))))

;; extracted for debugging purpose
(defun pm--select-existent-buffer-visibly (new-buffer)
  (let ((old-buffer (current-buffer))
        (point (point))
        (window-start (window-start))
        (visible (pos-visible-in-window-p))
        (vlm visual-line-mode)
        (ractive (region-active-p))
        ;; text-scale-mode
        (scale (and (boundp 'text-scale-mode) text-scale-mode))
        (scale-amount (and (boundp 'text-scale-mode-amount) text-scale-mode-amount))
        (hl-line (and (boundp 'hl-line-mode) hl-line-mode))
        (mkt (mark t))
        (bro buffer-read-only))

    (when hl-line
      (hl-line-mode -1))

    (pm--move-vars pm-move-vars-from-old-buffer old-buffer new-buffer)
    (pm--move-overlays old-buffer new-buffer)

    (switch-to-buffer new-buffer)
    (bury-buffer-internal old-buffer)

    (unless (eq bro buffer-read-only)
      (read-only-mode (if bro 1 -1)))
    (pm--adjust-visual-line-mode vlm)

    (when (and (boundp 'text-scale-mode-amount)
               (not (and (eq scale text-scale-mode)
                         (= scale-amount text-scale-mode-amount))))
      (if scale
          (text-scale-set scale-amount)
        (text-scale-set 0)))

    ;; fixme: what is the right way to do this ... activate-mark-hook?
    (if (not ractive)
        (deactivate-mark)
      (set-mark mkt)
      (activate-mark))

    ;; avoid display jumps
    (goto-char point)
    (when visible
      (set-window-start (get-buffer-window buffer t) window-start))

    (when hl-line
      (hl-line-mode 1))

    (run-hook-with-args 'polymode-switch-buffer-hook old-buffer new-buffer)
    (pm--run-hooks pm/polymode :switch-buffer-functions old-buffer new-buffer)
    (pm--run-hooks pm/chunkmode :switch-buffer-functions old-buffer new-buffer)))

(defun pm--move-overlays (from-buffer to-buffer)
  (with-current-buffer from-buffer
    (mapc (lambda (o)
            (unless (eq 'linum-str (car (overlay-properties o)))
              (move-overlay o (overlay-start o) (overlay-end o) to-buffer)))
          (overlays-in 1 (1+ (buffer-size))))))

(defun pm--move-vars (vars from-buffer &optional to-buffer)
  (let ((to-buffer (or to-buffer (current-buffer))))
    (unless (eq to-buffer from-buffer)
      (with-current-buffer to-buffer
        (dolist (var vars)
          (and (boundp var)
               (set var (buffer-local-value var from-buffer))))))))

(defun pm--adjust-visual-line-mode (vlm)
  (unless (eq visual-line-mode vlm)
    (if (null vlm)
        (visual-line-mode -1)
      (visual-line-mode 1))))

(defmethod pm-select-buffer ((config pm-polymode-multi-auto) &optional span)
  ;; :fixme: pm-get-span on multi configs returns config as last object of
  ;; span. This unnatural and confusing. Same problem with pm-indent-line
  (pm-select-buffer (pm--get-multi-chunk config span) span))

(defun pm--get-multi-chunk (config span)
  ;; fixme: cache somehow?
  (if (null (car span))
      (oref config -hostmode)
    (let ((type (car span))
          (proto (symbol-value (oref config :auto-innermode))))
      (save-excursion
        (goto-char (cadr span))
        (unless (eq type 'head)
          (let ((matcher (oref proto :head-reg)))
           (if (functionp matcher)
               (goto-char (car (funcall matcher -1)))
             (re-search-backward matcher nil 'noerr))))
        (let* ((str (or
                     ;; a. try regexp matcher
                     (and (oref proto :retriever-regexp)
                          (re-search-forward (oref proto :retriever-regexp) nil t)
                          (match-string-no-properties (oref proto :retriever-num)))
                     ;; b. otherwise function (fixme: these should be merged)
                     (and (oref proto :retriever-function)
                          (funcall (oref proto :retriever-function)))))
               (mode (pm--get-mode-symbol-from-name str 'no-fallback)))
          (if mode
              ;; Inferred body MODE serves as ID; this not need be the
              ;; case in the future and a generic id getter might replace
              ;; it. Currently head/tail/body indirect buffers are shared
              ;; across chunkmodes. This currently works ok. A more
              ;; general approach would be to track head/tails/body with
              ;; associated chunks. Then for example r hbt-chunk and elisp
              ;; hbt-chunk will not share head/tail buffers. There could
              ;; be even two r hbt-chunks with providing different
              ;; functionality and thus not even sharing body buffer.
              (let ((name (concat (object-name-string proto) ":" (symbol-name mode))))
                (or
                 ;; a. loop through installed inner modes
                 (loop for obj in (oref config -auto-innermodes)
                       when (equal name (object-name-string obj))
                       return obj)
                 ;; b. create new
                 (let ((innermode (clone proto name :mode mode)))
                   (object-add-to-list config '-auto-innermodes innermode)
                   innermode)))
            ;; else, use hostmode
            (oref pm/polymode -hostmode)))))))


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
      span)))

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

nil   - pos is between point-min and head-reg, or between tail-reg and point-max
body  - pos is between head-reg and tail-reg (exclusively)
head  - head span
tail  - tail span"
  ;; ! start of the span is part of the span !
  (save-restriction
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
          (t (error "head and tail matchers should be either regexp strings or functions")))))


;;; INDENT
(defun pm-indent-line-dispatcher ()
  "Dispatch methods indent methods on current span."
  (let ((span (pm-get-innermost-span))
        (inhibit-read-only t))
    (pm-indent-line (car (last span)) span)))

(defgeneric pm-indent-line (&optional chunkmode span)
  "Indent current line.
Protect and call original indentation function associated with
the chunkmode.")

(defun pm--indent-line (span)
  (let (point)
    (save-current-buffer
      (pm-set-buffer span)
      (pm-with-narrowed-to-span span
        (funcall pm--indent-line-function-original)
        (setq point (point))))
    (goto-char point)))

(defmethod pm-indent-line ((chunkmode pm-chunkmode) &optional span)
  (pm--indent-line span))

(defmethod pm-indent-line ((chunkmode pm-hbtchunkmode) &optional span)
  "Indent line in inner chunkmodes.
When point is at the beginning of head or tail, use parent chunk
to indent."
  (let ((pos (point))
        (span (or span (pm-get-innermost-span)))
        delta)
    (unwind-protect
        (cond

         ;; 1. in head or tail (we assume head or tail fit in one line for now)
         ((or (eq 'head (car span))
              (eq 'tail (car span)))
          (goto-char (nth 1 span))
          (setq delta (- pos (point)))
          (when (not (bobp))
            (let ((prev-span (pm-get-innermost-span (1- pos))))
              (if (and (eq 'tail (car span))
                       (eq (point) (save-excursion (back-to-indentation) (point))))
                  ;; if tail is first on the line, indent as head
                  (indent-to (pm--head-indent prev-span))
                (pm--indent-line prev-span)))))

         ;; 2. body
         (t
          (back-to-indentation)
          (if (> (nth 1 span) (point))
              ;; first body line in the same line with header (re-indent at indentation)
              (pm-indent-line-dispatcher)
            (setq delta (- pos (point)))
            (pm--indent-line span)
            (let ((fl-indent (pm--first-line-indent span)))
              (if fl-indent
                  (when (bolp)
                    ;; Not first line. Indent only when original indent is at
                    ;; 0. Otherwise it's a continuation indentation and we assume
                    ;; the original function did it correctly with respect to
                    ;; previous lines.
                    (indent-to fl-indent))
                ;; First line. Indent with respect to header line.
                (indent-to
                 (+ (- (point) (point-at-bol)) ;; non-0 if code in header line
                    (pm--head-indent span) ;; indent with respect to header line
                    (oref chunkmode :indent-offset))))))))
      ;; keep point on same characters
      (when (and delta (> delta 0))
        (goto-char (+ (point) delta))))))

(defun pm--first-line-indent (&optional span)
  (save-excursion
    (let ((pos (point)))
      (goto-char (nth 1 (or span (pm-get-innermost-span))))
      (goto-char (point-at-eol))
      (skip-chars-forward " \t\n")
      (let ((indent (- (point) (point-at-bol))))
        (when (< (point-at-eol) pos)
          indent)))))

(defun pm--head-indent (&optional span)
  (save-excursion
    (goto-char (nth 1 (or span (pm-get-innermost-span))))
    (back-to-indentation)
    (- (point) (point-at-bol))))

(defmethod pm-indent-line ((config pm-polymode-multi-auto) &optional span)
  ;; fixme: pm-polymode-multi-auto is not a chunk, pm-get-innermost-span should
  ;; not return it in the first place
  ;; (pm-set-buffer span)
  ;; (pm-indent-line pm/chunkmode span))
  (pm-indent-line (pm--get-multi-chunk config span) span))


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
