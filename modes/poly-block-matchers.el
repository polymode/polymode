(defmacro pm-create-block-matchers (name regex)
  (let* ((head-name (intern (format "pm-%s-head-matcher" name)))
         (tail-name (intern (format "pm-%s-tail-matcher" name))))
    `(progn
       (defun ,head-name (ahead)
         (when (re-search-forward ,regex nil t ahead)
           (cons (match-beginning 0) (match-end 0))))

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

(provide 'poly-block-matchers)
