
(defun pm-polymode-one (&rest slots)
  "[obsolete] Please use `pm-polymode' instead."
  (display-warning 'polymode "`pm-polymode-one' is obsolete. Creating `pm-polymode' instead.")
  (let* ((name (if (not (keywordp (car slots)))
                   (pop slots)
                 ""))
         (innermodes)
         (new-slots (cl-loop for p on slots by #'cddr
                             if (eq (car p) :innermode) do (push (cadr p) innermodes)
                             else if (eq (car p) :innermodes) do (setq innermodes (append (cadr p) innermodes))
                             else append (list (car p) (cadr p))))
         (new-slots (append (list name :innermodes innermodes) new-slots)))
    (apply #'pm-polymode new-slots)))

(defun pm-polymode-multi (&rest slots)
  "[obsolete] Please use `pm-polymode' instead."
  (display-warning 'polymode "`pm-polymode-multi' is obsolete. Creating `pm-polymode' instead.")
  (apply #'pm-polymode slots))

(defun pm-polymode-multi-auto (&rest slots)
  "[obsolete] Please use `pm-polymode' instead."
  (display-warning 'polymode "`pm-polymode-multi-auto' is obsolete. Creating `pm-polymode' instead.")
  (let* ((name (if (not (keywordp (car slots)))
                   (pop slots)
                 ""))
         (innermodes)
         (new-slots (cl-loop for p on slots by #'cddr
                             if (eq (car p) :auto-innermode) do (push (cadr p) innermodes)
                             else if (eq (car p) :innermodes) do (setq innermodes (append (cadr p) innermodes))
                             else append (list (car p) (cadr p))))
         (new-slots (append (list name :innermodes innermodes) new-slots)))
    (apply #'pm-polymode new-slots)))


(defmethod clone ((obj pm-polymode) &rest slots)
  "[obsolete] Please use `pm-polymode' instead."
  (let* ((name (when (not (keywordp (car slots)))
                 (pop slots)))
         (innermodes (oref obj :innermodes))
         (obsolete-names)
         (new-slots (cl-loop for p on slots by #'cddr
                             if (eq (car p) :innermode) do (progn (push (cadr p) innermodes)
                                                                  (push (car p) obsolete-names))
                             else if (eq (car p) :auto-innermode) do (progn (push (cadr p) innermodes)
                                                                            (push (car p) obsolete-names))
                             else if (eq (car p) :innermodes) do (setq innermodes (append (cadr p) innermodes))
                             else append (list (car p) (cadr p))))
         (new-slots (append `(,@(when name (list name)) :innermodes ,innermodes) new-slots)))
    (when (> (length obsolete-names) 0)
      (display-warning 'polymode (format "Obsolete slot names %s. Use :innermodes instead." obsolete-names)))
    (apply #'call-next-method obj new-slots)))

(defun pm-bchunkmode (&rest slots)
  "[obsolete] Please use `pm-host-chunkmode' instead."
  (display-warning 'polymode "`pm-bchunkmode' is obsolete. Creating `pm-host-chunkmode' instead.")
  (apply #'pm-host-chunkmode slots))

(defun pm-hbtchunkmode (&rest slots)
  "[obsolete] Please use `pm-inner-chunkmode' instead."
  (display-warning 'polymode "`pm-hbtchunkmode' is obsolete. Creating `pm-inner-chunkmode' instead.")
  (let* ((name (if (not (keywordp (car slots)))
                   (pop slots)
                 ""))
         (new-slots (cl-loop for p on slots by #'cddr
                             if (eq (car p) :head-matcher) append (list :head-matcher (cadr p))
                             else if (eq (car p) :tail-matcher) append (list :tail-matcher (cadr p))
                             else append (list (car p) (cadr p)))))
    (apply #'pm-inner-chunkmode (cons name new-slots))))

(defun pm-hbtchunkmode-auto (&rest slots)
  "[obsolete] Please use `pm-inner-auto-chunkmode' instead."
  (display-warning 'polymode "`pm-hbtchunkmode-auto' is obsolete. Creating `pm-inner-auto-chunkmode' instead.")
  (let* ((name (if (not (keywordp (car slots)))
                   (pop slots)
                 ""))
         (mode-matcher (cons nil 1))
         (new-slots (cl-loop for p on slots by #'cddr
                             if (eq (car p) :retriever-num) do (setcdr mode-matcher (cadr p))
                             else if (eq (car p) :retriever-regexp) do (if (stringp (cadr p))
                                                                           (setcar mode-matcher (cadr p))
                                                                         (setq mode-matcher (cadr p)))
                             else if (eq (car p) :retriever-function) do (setq mode-matcher (cadr p))
                             else if (eq (car p) :head-matcher) append (list :head-matcher (cadr p))
                             else if (eq (car p) :tail-matcher) append (list :tail-matcher (cadr p))
                             else append (list (car p) (cadr p))))
         (new-slots (append (list name :mode-matcher mode-matcher) new-slots)))
    (apply #'pm-inner-auto-chunkmode new-slots)))

(provide 'polymode-obsolete)
