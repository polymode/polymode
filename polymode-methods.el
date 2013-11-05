
;;;; INTERFACE
(defgeneric pm/initialize (config)
  "Initialize current buffer with CONFIG.

First initialize the :base-submode and :inner-submodes slots of
CONFIG object ...

Current buffer is setup as the base buffer.")
;; (defmethod pm/initialize ((config pm-config))
;;   (pm--setup-buffer (current-buffer)))

(defmethod pm/initialize ((config pm-config))
  ;; fixme: reinstalation leads to infloop of pm--fontify-region-original and others ... 
  ;; On startup with local auto vars emacs reinstals the mode twice .. waf?
  ;; For time baing never reinstall twice
  (unless pm/config
    (let* ((submode (clone (symbol-value (oref config :base-submode-name))
                           :buffer (current-buffer)))
           ;; set if nil, to allow unspecified base submodes to be used in minor modes
           (base-mode (or (oref submode :mode)
                          (oset submode :mode major-mode))))
      (unless (or (eq major-mode base-mode)
                  (eq polymode-major-mode base-mode))
        (let ((polymode-mode t)) ;;major-modes might check it 
          (funcall base-mode)))
      ;; fixme: maybe: inconsistencies?
      ;; 1)  not calling pm/install-buffer on base-buffer
      ;; But, we are not creating/installing a new buffer here .. so it is a
      ;; different thing .. and is probably ok
      ;; 2)  not calling config's :minor-mode-name (polymode function).
      ;; But polymode function calls pm/initialize... so I guess it is ok
      (oset config :base-submode submode)
      (setq pm/config config)
      (setq pm/submode submode)
      (setq pm/type 'base)
      ;; base specific config and setup
      (add-hook 'flyspell-incorrect-hook 'pm--flyspel-dont-highlight-in-submodes nil t)
      ;; general setup
      (pm--setup-buffer))))
  
(defmethod pm/initialize ((config pm-config-one))
  (call-next-method)
  (eval `(oset config :inner-submodes
               (list (clone ,(oref config :inner-submode-name))))))

(defmethod pm/initialize ((config pm-config-multi))
  (call-next-method)
  (oset config :inner-submodes
        (mapcar (lambda (sub-name)
                  (clone (symbol-value sub-name)))
                (oref config :inner-submode-names))))

(defgeneric pm/get-buffer (submode &optional span-type)
  "Get the indirect buffer associated with SUBMODE and
SPAN-TYPE. Should return nil if buffer has not yet been
installed. Also see `pm/get-span'.")

(defmethod pm/get-buffer ((submode pm-submode) &optional type)
  (oref submode :buffer))

(defmethod pm/get-buffer ((submode pm-inner-submode) &optional type)
  (cond ((eq 'body type) (oref submode :buffer))
        ((eq 'head type) (oref submode :head-buffer))
        ((eq 'tail type) (oref submode :tail-buffer))
        (t (error "Don't know how to select buffer of type" type
                  "for submode" (object-name submode)
                  "of class" (class-of submode)))))

(defgeneric pm/select-buffer (submode span)
  "Ask SUBMODE to select (make current) its indirect buffer
corresponding to the type of the SPAN returned by
`pm/get-span'.")

(defmethod pm/select-buffer ((submode pm-submode) span)
  "Select the buffer associated with SUBMODE.
Install a new indirect buffer if it is not already installed.

For this method to work correctly, SUBMODE's class should define
`pm/install-buffer' and `pm/get-buffer' methods."
  (let* ((type (car span))
         (buff (pm/get-buffer submode type)))
    (unless (buffer-live-p buff)
      (pm/install-buffer submode type)
      (setq buff (pm/get-buffer submode type)))
    (pm--select-buffer buff)))

(defmethod pm/select-buffer ((submode pm-inner-submode) span)
  (call-next-method)
  (pm--transfer-vars-from-base))

(defun pm-get-mode-symbol-from-name (str)
  "Default mode function guesser.
Return major mode function constructed from STR by appending
'-mode' if needed. If the constructed symbol is not a function
return an error."
  (let ((mname (if (string-match-p "-mode$" str)
                   str
                 (concat str "-mode"))))
         (pm--get-available-mode (intern mname))))

(defmethod pm/select-buffer ((config pm-config-multi-auto) &optional span)
  (if (null (car span))
      (pm/select-buffer (oref config :base-submode) span)
    (let ((type (car span))
          (proto (symbol-value (oref config :auto-submode-name)))
          submode)
      (save-excursion
        (goto-char (cadr span))
        (unless (eq type 'head)
          (re-search-backward (oref proto :head-reg) nil 'noerr))
        (re-search-forward (oref proto :retriever-regexp))
        (let* ((str (or (match-string-no-properties (oref proto :retriever-num))
                        (error "retriever subexpression didn't match")))
               (name (concat "auto-submode:" str)))
          (setq submode
                (or (loop for obj in (oref config :auto-submodes)
                          when  (equal name (object-name-string obj))
                          return obj)
                    (let ((new-obj (clone proto name
                                          :mode (pm-get-mode-symbol-from-name str))))
                      (object-add-to-list config :auto-submodes new-obj)
                      new-obj)))))
      (pm/select-buffer submode span))))


(defgeneric pm/install-buffer (submode &optional type)
  "Ask SUBMODE to install an indirect buffer corresponding to
span TYPE. Should return newly installed/retrieved buffer.")

(defmethod pm/install-buffer ((submode pm-submode) &optional type)
  "Independently on the TYPE call `pm/create-indirect-buffer'
create and install a new buffer in slot :buffer of SUBMODE."
  (oset submode :buffer
        (pm--create-submode-buffer-maybe submode type)))

(defmethod pm/install-buffer ((submode pm-inner-submode) type)
  "Depending of the TYPE install an indirect buffer into
slot :buffer of SUBMODE. Create this buffer if does not exist."
  (pm--set-submode-buffer submode type
                          (pm--create-submode-buffer-maybe submode type)))

(defun pm--create-submode-buffer-maybe (submode type)
  ;; assumes pm/config is set
  (let ((mode (pm--get-submode-mode submode type)))
    (or (pm--get-indirect-buffer-of-mode mode)
        (let ((buff (pm--create-indirect-buffer mode)))
           (with-current-buffer  buff
             (setq pm/submode submode)
             (setq pm/type type)
             (pm--setup-buffer)
             (funcall (oref pm/config :minor-mode-name))
             buff)))))

(defgeneric pm/get-adj-face (submode &optional type))
(defmethod pm/get-adj-face ((submode pm-submode) &optional type)
  (oref submode :adj-face))
(defmethod pm/get-adj-face ((submode pm-inner-submode) &optional type)
  (setq type (or type pm/type))
  (cond ((eq type 'head)
         (oref submode :head-adj-face))
        ((eq type 'tail)
         (if (eq 'head (oref pm/submode :tail-adj-face))
             (oref pm/submode :head-adj-face)
           (oref pm/submode :tail-adj-face)))
        (t (oref pm/submode :adj-face))))

(defgeneric pm/get-span (submode &optional pos)
  "Ask a submode for the span at point.
Return a list of three elements (TYPE BEG END OBJECT) where TYPE
is a symbol representing the type of the span surrounding
POS (head, tail, body, inline etc). BEG and END are the
coordinates of the span. OBJECT is a sutable object which is
'responsable' for this span. That is, OBJECT could be dispached
upon with `pm/select-buffer' or other methods form the interface.

Should return nil if there is no SUBMODE specific span around POS.")

(defmethod pm/get-span (submode &optional pos)
  "Simply return nil. Base mode usually do/can not compute the span"
  nil)

(defmethod pm/get-span ((config pm-config) &optional pos)
    "Apply pm/get-span on every element of submodes slot of config object.
Return a cons (submode . span), for which START is closest to
POS (and before it); i.e. the innermost span.  POS defaults to
point."
    ;; fixme: base should be last, to take advantage of the submodes computation
    (let ((smodes (cons (oref config :base-submode) 
                        (oref config :inner-submodes)))
          (start (point-min))
          (end (point-max))
          (pos (or pos (point)))
          span val)
      (save-restriction
        (widen)
        (dolist (sm smodes)
          (setq val (pm/get-span sm pos))
          (when (and val
                     (or (> (nth 1 val) start)
                         (< (nth 2 val) end)))
            (if (or (car val)
                    (null span))
                (setq span val
                      start (nth 1 val)
                      end (nth 2 val))
              ;; nil car means outer submode (usually base). And it can be an
              ;; intersection of spans returned by 2 different neighbour inner
              ;; submodes. See rapport mode for an example
              (setq start (max (nth 1 val)
                               (nth 1 span))
                    end (min (nth 2 val)
                             (nth 2 span)))
              (setcar (cdr span) start)
              (setcar (cddr span) end)
              ))))
      (unless (and (<= start end) (<= pos end) (>= pos start))
        (error "Bad polymode selection: %s, %s"
               (list start end) pos))
      ;; fixme: why is this here?
      ;; (if (= start end)
      ;;     (setq end (1+ end)))
      (when (and span
                 (null (car span))) ; submodes can compute the base span by returning nil
        (setcar (last span) (oref config :base-submode)))
      span))

;; No need for this one so far. Basic method iterates through :inner-submodes
;; anyhow.
;; (defmethod pm/get-span ((config pm-config-multi) &optional pos))

(defmethod pm/get-span ((config pm-config-multi-auto) &optional pos)
  (let ((span-other (call-next-method))
        (proto (symbol-value (oref config :auto-submode-name))))
    (if (oref proto :head-reg)
        (let ((span (pm--span-at-point (oref proto :head-reg)
                                       (oref proto :tail-reg)
                                       pos)))
          (if (and span-other
                   (or (> (nth 1 span-other) (nth 1 span))
                       (< (nth 2 span-other) (nth 2 span))))
              span-other
            (append span (list config)))) ;fixme: this returns config as last object
      span-other)))

(defmethod pm/get-span ((submode pm-inner-submode) &optional pos)
  "Return a list of the form (TYPE POS-START POS-END SELF).
TYPE can be 'body, 'head or 'tail. SELF is just a submode object
in this case."
  (with-slots (head-reg tail-reg head-mode tail-mode) submode
    (let* ((span (pm--span-at-point head-reg tail-reg pos))
           (type (car span)))
      (when (or (and (eq type 'head) (eq head-mode 'base))
                (and (eq type 'tail) (or (eq tail-mode 'base)
                                         (and (null tail-mode)
                                              (eq head-mode 'base)))))
        (setcar span nil))
      (append span (list submode)))))

;;; UTILS
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
  ;; efficent reg-reg lookup with only 2 searches
  (save-excursion
    (let* ((pos (point))
           (reg (concat "\\(?1:\\(" tail-matcher "\\)\\)\\|\\(?2:\\(" head-matcher "\\)\\)"))
           (pos1-end (if (re-search-backward reg nil t)
                         (match-end 0)))
           (pos1-tail? (or (null pos1-end) (match-end 1))) ;; consider point-min as a tail
           (pos1-end (goto-char (or pos1-end  (point-min))))
           (pos2-start (if (re-search-forward reg nil t)
                           (match-beginning 0)))
           (pos2-end (and pos2-start (match-end 0)))
           (pos2-tail? (and pos2-start (match-end 1)))
           (pos2-start (or pos2-start (point-max)))) ;consider pointmax as head
      (if (or (< pos pos2-start)
              (eq pos (point-max)))
          ;; inside doc or chunk body
          (if pos1-tail? 
              (list nil pos1-end pos2-start) ;doc
            (list 'body pos1-end pos2-start)) ; chunk body
        ;; else inside head or tail
        (if (< pos pos2-end) ; <- this one should be always true
            (if pos2-tail?
                (list 'tail pos2-start pos2-end)
              (list 'head pos2-start pos2-end)))
        ))))

(defun pm--span-at-point (head-matcher tail-matcher &optional pos)
  "Basic span detector with head/tail.

HEAD-MATCHER and TAIL-MATCHER can be regexp or functions
returning (cons beg end) and accepting one argument AHEAD that
can be either 1 or -1 for either forward or backward search.

Return (type span-start span-end) where type is one of the
follwoing symbols:

nil - pos is between (tail-reg or point-min) and (head-reg or point-max)
body - pos is between head-reg and (tail-reg or point-max)
head -  head span
tail -  tail span"
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
            (lambda (ahead) (pm--default-matcher hd-matcher ahead))
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



;;; INDENT-LINE

(defgeneric pm/indent-line (&optional submode span)
  "Indent current line.
Protect and call original indentation function associated with
the submode.")

(defun pm--indent-line (span)
  ;; istr is auto-indent string
  (unwind-protect
      (save-restriction
          (pm--comment-region  1 (nth 1 span))
          (pm/narrow-to-span span)
          (funcall pm--indent-line-function-original))
      (pm--uncomment-region 1 (nth 1 span))))

(defmethod pm/indent-line ()
  (let ((span (pm/get-innermost-span)))
    (pm/indent-line (car (last span)) span)))

(defmethod pm/indent-line ((submode pm-submode) &optional span)
  (pm--indent-line span))
  
(defmethod pm/indent-line ((submode pm-inner-submode) &optional span)
  "Indent line in inner submodes.
When point is at the beginning of head or tail, use parent chunk
to indent."
  ;; sloppy work,
  ;; assumes multiline chunks and single-line head/tail
  ;; assumes current buffer is the correct buffer
  (let ((pos (point))
        shift delta)
    (cond ((or (eq 'head (car span))
               (eq 'tail (car span)))
            ;; use parent's indentation function
           (back-to-indentation)
           (setq delta (- pos (point)))
           (backward-char)
           (let ((parent-span (pm/get-innermost-span)))
             (pm/select-buffer (car (last parent-span)) parent-span)
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
            (setq delta (- (point) (point-at-bol)))
            (beginning-of-line)
            (indent-to shift)
            (goto-char (+ (point) delta))))))

(defmethod pm/indent-line ((submode pm-config-multi-auto) &optional span)
  (pm/select-buffer submode span)
  (pm/indent-line pm/submode span))

(defun pm--get-head-shift (span)
  (save-excursion
    (goto-char (cadr span))
    (back-to-indentation)
    (- (point) (point-at-bol))))
                     
;; (defun pm--test-ff ()
;;   (interactive)
;;   (let* ((hd-reg "<!--[ \t]*begin.rcode")
;;          (tl-reg "end.rcode[ \t]*-->")
;;          (span (pm--span-at-point
;;                 (lambda (ahead) (pm--default-matcher hd-reg ahead))
;;                 (lambda (ahead) (pm--default-matcher tl-reg ahead)))))
;;     (ess-blink-region (cadr span) (caddr span))
;;     (message "%s" span)))

;; (defun pm--test-rf ()
;;   (interactive)
;;   (let ((span (pm--span-at-point "<!--[ \t]*begin.rcode"
;;                                  (lambda (ahead)
;;                                    (pm--default-matcher "end.rcode[ \t]*-->" ahead)))))
;;     (ess-blink-region (cadr span) (caddr span))
;;     (message "%s" span)))

;; (defun pm--test-rr ()
;;   (interactive)
;;   (let ((span (pm--span-at-point "<!--[ \t]*begin.rcode"
;;                                  "end.rcode[ \t]*-->")))
;;     (ess-blink-region (cadr span) (caddr span))
;;     (message "%s" span)))

              

(provide 'polymode-methods)
