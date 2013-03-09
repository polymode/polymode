
(defmethod pm/install-config ((object pm-config))
  "Clone OBJECT and bind it to `pm/local-config'.
Iterate over submodes slot and move those which return non-nil
`pm/applies-p' into active-submodes slot."
  (setq pm/local-config (clone object))
  (dolist (sm (oref pm/local-config submodes))
    (when (pm/applies-p sm)
      (object-add-to-list pm/local-config active-submodes sm t))))


(defgeneric pm/applies-p (object)
  "Check if an OBJECT applies to the current context (buffer and mode).")

(defmethod pm/applies-p ((submode pm-submode))
  "Check if  SUBMODE appllies to the current buffer.
Default method match :extensions slots of SUBMODE with the
current file's extension."
  (let ((EXT (upcase (file-name-extension (buffer-file-name))))
        out) 
    ;; todo: should be regexp
    (member* EXT (oref submode :extensions)  :test 'equal :key 'upcase)))


;;;; INTERFACE
(defgeneric pm/init-submodes (config)
  "Initialize the :submodes slot of CONFIG object")

(defgeneric pm/get-span (submode &optional pos)
  "Ask a submode for the span at point.
Return a list of three elements (TYPE BEG END) where TYPE is a
symbol representing the type of the span surrounding POS (header,
tail, body, inline etc). BEG and END are the coordinates of the span.

Should return nil if there is no SUBMODE specific span around POS.")

(defgeneric pm/get-buffer (submode &optional span-type)
  "Get the indirect buffer associated with SUBMODE and
SPAN-TYPE. Should return nil if buffer has not yet been
installed. Also see `pm/get-span'.")

(defgeneric pm/install-buffer (submode &optional span-type)
  "Ask SUBMODE to install an indirect buffer corresponding to
SPAN-TYPE.")

(defgeneric pm/select-buffer (submode type)
  "Ask SUBMODE to select (make current) it's indirect buffer
corresponding to the TYPE of the span returned by
`pm/get-span'.")

(defmethod pm/select-buffer ((submode pm-submode) type)
  "Check and install new indirect buffer if it is not already installed.

For this method to work correctly, SUBMODE class should define
`pm/install-buffer' and `pm/get-buffer' methods.
"
  (let ((buff (pm/get-buffer submode type)))
    (unless buff
      (pm/install-buffer submode type)
      (setq buff (pm/get-buffer submode type)))
    (pm--select-buffer buff)
    ))


;;;; SUBMODE/CONFIG-SIMPLE METHODS
(defmethod pm/init-submodes ((config pm-config-simple))
  (eval `(oset config :submodes (list (clone ,(oref config :default-submode))))))
   
(defmethod pm/get-span ((submode pm-submode-simple) &optional pos)
  "Span type can be 'body, 'head or 'tail."
  (with-slots (header-reg tail-reg header-mode tail-mode) submode
    (let* ((span (pm--span-at-point header-reg tail-reg pos))
           (type (car span)))
      (when (or (and (eq type 'head) (eq header-mode 'base))
                (and (eq type 'tail) (or (eq tail-mode 'base)
                                         (and (null tail-mode)
                                              (eq header-mode 'base)))))
        (setcar span nil))
      span)))

(defmethod pm/get-buffer ((submode pm-submode-simple) &optional type)
  (oref submode :buffer))

(defmethod pm/install-buffer ((submode pm-submode-simple) &optional type)
  "Independently on the TYPE call `pm/create-indirect-buffer'
and install in slot :buffer of SUBMODE."
  (oset submode :buffer
        (pm/create-indirect-buffer (oref submode mode))))



;;; UTILS
(defun pm--span-at-point (head-reg tail-reg &optional pos)
  "Basic span detector with head/tail.

Return (type span-start span-end) where type is one of the
follwoing symbols:

nil - pos is between (tail-reg or point-min) and (head-reg or point-max)
body - pos is between head-reg and (tail-reg or point-max)
head -  head span
tail -  tail span"
  (save-excursion
    (save-restriction
      (widen)
      (setq pos (or pos (point)))
      (goto-char pos)
      (let* ((reg (concat "\\(?1:\\(" tail-reg "\\)\\)\\|\\(?2:\\(" head-reg "\\)\\)"))
             (pos1-end (if (re-search-backward reg nil t)
                           (match-end 0)))
             (pos1-tail? (or (null pos1-end) (match-end 1))) ;; consider point-min as a tail
             (pos1-end (goto-char (or pos1-end  (point-min))))
             (pos2-start (if (re-search-forward reg nil t)
                             (match-beginning 0)))
             (pos2-end (and pos2-start (match-end 0)))
             (pos2-tail? (and pos2-start (match-end 1)))
             (pos2-start (or pos2-start (point-max)))) ;consider pointmax as head
        (if (<= pos pos2-start) ; inside doc or chunk body
            (if pos1-tail? 
                (list nil pos1-end pos2-start) ;doc
              (list 'body pos1-end pos2-start)) ; chunk body
          ;; else inside head or tail
          (if (< pos pos2-end) ;; just in case
              (if pos2-tail?
                  (list 'tail pos2-start pos2-end)
                (list 'head pos2-start pos2-end)))
          )))))

(let ((ess-blink-delay 1)
      (span (pm/-span-at-point-with-headtail  "^<<\\(.*\\)>>=" "^\\(@ +%def .*\\)$\\|\\(@[ \n]\\)")))
  (ess-blink-region (cadr span) (nth 2 span)))

(pm/-span-at-point-with-headtail  "^<<\\(.*\\)>>=" "^\\(@ +%def .*\\)$\\|\\(@[ \n]\\)")
;; (pm/-span-at-point "^<<\\(.*\\)>>=" "^\\(@ +%def .*\\)$\\|\\(@[ \n]\\)")

;; (defun pm/-span-at-point (head-reg tail-reg &optional pos)
;;   "Basic span detector.

;; Return (type span-start span-end) where type is one of the

;; nil - pos is between tail-reg-end and (head-reg-start or point-max)
;; t  - pos is between head-reg-start and (tail-reg-end or point-max)
;; "
;;   (save-excursion
;;     (save-restriction
;;       (widen)
;;       (setq pos (or pos (point)))
;;       (goto-char pos)
;;       (let ((reg (concat "\\(?1:\\(" tail-reg "\\)\\)\\|\\(?2:\\(" head-reg "\\)\\)"))
;;             (out '(nil nil nil nil nil))
;;             (mdata (and (re-search-backward reg nil t)
;;                         (match-data t))))
;;         (if (and mdata (nth 4 mdata))
;;             ;; found head
;;             (progn
;;               (setcar (cdr out) (nth 4 mdata)) ;; head start
;;               (setcar (cddr out) (nth 5 mdata)) ;; head end
;;               (goto-char (nth 5 mdata)))
;;           ;; found tail or nothing
;;           (goto-char (or (nth 3 mdata)  ; end of previous tail 
;;                          (point-min)))
;;           (if (and (re-search-forward head-reg) ;; pos might be in the head
;;                    (> pos (match-beginning 0))) 
;;               (progn
;;                 (setcar (cdr out) (match-beginning 0))
;;                 (setcar (cddr out)
;;                         (goto-char (mach-end 0))))
;;             (setq out nil)))
;;         (when out
;;           ;; head coordinates have been recorded and positioned at the head-end
;;           (if (re-search-forward tail-reg)
;;               (if (< pos (match-end 0))
;;                   (progn 
;;                     (setcar (last out 2) (match-beginning 0))
;;                     (setcar (last out) (match-end 0)))
;;                 ;; position of end is before pos. (should not happen if regs are exclusive)
;;                 (setq out nil))
;;             ;; point-max
;;             (setcar (last out 2) (point-max))
;;             (setcar (last out) (point-max))))
;;         out))))





;; ;;         (head-start (match-beginning 2))



;; ;;      ;; this is all match data, simplify?
;; ;;      (pos1-head-start (and pos1-end (match-end 2)))
;; ;;      (pos1-tail-start (and pos1-end (match-end 1)))
;; ;;      (pos1-end (goto-char (or pos1-end (point-min))))
;; ;;      (pos1-start (or pos1-tail-start pos1-head-start))

;; ;;      (pos2-end (if (re-search-forward reg nil t)
;; ;;                    (match-end 0)))
;; ;;      (pos2-head-start (and pos2-end (match-start 2)))
;; ;;      (pos2-tail-start (and pos2-end (match-start 1)))
;; ;;      (pos2-start (or pos2-tail-start pos2-head-start))
;; ;;      (pos2-end (or pos2-end (point-max)))
;; ;;      )
;; ;; (if (< pos pos2-end) ; inside doc or chunk
;; ;;     (cond
;; ;;      (pos2-tail-start
;; ;;       (if pos1-head-start
;; ;;           (list t pos1-head-start pos2-end)
;; ;;         (list t pos1-end pos2-end))) ;; incomplete chunk, take everything from previous pos
;; ;;      (pos2-head-start
;; ;;       (list nil pos1-end pos2-head-start)) ;; doc
;; ;;      (pos1-head-start
;; ;;       (list t pos1-head-start pos2-end)) ;; incomplete chunk, take everything to the end
;; ;;      (pos1-tail-start
;; ;;       (list nil pos1-end))
;; ;;      (pos2-tail-start )
;; ;;           (if pos1-start
;; ;;               (if pos2-tail-start
;; ;;       (list t new-pos (or pos2-start (point-max)))) ; chunk body
;; ;;   (if (< pos pos2-end) ;; just in case
;; ;;       (if pos2-tail?
;; ;;           (list 2 pos2-start pos2-end)
;; ;;         (list 1 pos2-start pos2-end)))
;; ;;   )))))
