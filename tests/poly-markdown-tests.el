
(require 'markdown-mode)
(require 'poly-markdown)
(require 'polymode-test)

(setq python-indent-offset 4
      python-indent-guess-indent-offset nil)

(ert-deftest poly-markdown/headings ()
  (pm-test-poly-lock poly-markdown-mode "markdown.md"
    ((insert-1 61)
     (insert " ")
     (pm-test-chunks)
     (delete-backward-char 1))
    ((delete-2 1842)
     (backward-kill-word 1))
    ((insert-new-line-3 2173)
     (insert "\n")
     (pm-test-chunks)
     (delete-backward-char 1))))

(ert-deftest poly-markdown/headings2 ()
  (pm-test-poly-lock poly-markdown-mode "markdown.md"
    ((insert-1 ("^## Intro" beg))
     (insert " ")
     (pm-test-chunks)
     (delete-backward-char 1))
    ((delete-2 "^2. Blockquotes")
     (backward-kill-word 1))
    ((insert-new-line-3 ("^3. Two Inline" end))
     (insert "\n")
     (pm-test-chunks)
     (delete-backward-char 1))))

(ert-deftest poly-markdown/fenced-code ()
  (pm-test-poly-lock poly-markdown-mode "markdown.md"
    ((delete-fortran-print (23))
     (forward-word)
     (delete-backward-char 1))
    ((insert-ada-hello (51))
     (insert "\"hello!\"\n")
     (indent-for-tab-command))
    ((insert-lisp-arg "&rest forms")
     (backward-sexp 2)
     (insert "first-arg "))
    ((python-kill-line (130))
     (kill-line 3))
    ((elisp-kill-sexp ("(while (setq retail" beg))
     (kill-sexp))
    ((elisp-kill-defun ("(defun delete-dups" beg))
     (kill-sexp))))

(ert-deftest poly-markdown/spans-at-borders ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (pm-map-over-spans
     (lambda ()
       (let* ((sbeg (nth 1 *span*))
              (send (nth 2 *span*))
              (range1 (pm-get-innermost-range sbeg))
              (range2 (pm-get-innermost-range send)))
         (should (eq sbeg (car range1)))
         (should (eq send (cdr range1)))
         (unless (eq send (point-max))
           (should (eq send (car range2)))))))))

(ert-deftest poly-markdown/spans-at-narrowed-borders ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (pm-map-over-spans
     (lambda ()
       (pm-with-narrowed-to-span *span*
         (let* ((range1 (pm-get-innermost-range (point-min)))
                (range2 (pm-get-innermost-range (point-max))))
           (should (eq (car range1) (point-min)))
           (should (eq (cdr range1) (point-max)))
           (should (eq (car range2) (point-min)))
           (should (eq (cdr range2) (point-max)))))))))

(ert-deftest poly-markdown/spans-at-narrowed-borders ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (pm-map-over-spans
     (lambda ()
       (pm-with-narrowed-to-span *span*
         (let* ((range1 (pm-get-innermost-range (point-min)))
                (range2 (pm-get-innermost-range (point-max))))
           (should (eq (car range1) (point-min)))
           (should (eq (cdr range1) (point-max)))
           (should (eq (car range2) (point-min)))
           (should (eq (cdr range2) (point-max)))))))))

(ert-deftest poly-markdown/narrowed-spans ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (narrow-to-region 60 200)
    (let ((span (pm-innermost-span (point-min))))
      (should (eq (car span) nil))
      (should (= (nth 1 span) 60))
      (should (= (nth 2 span) 200)))
    (widen)
    (narrow-to-region 60 500)
    (let ((span (pm-innermost-span (point-min))))
      (should (= (nth 1 span) 60))
      (should (= (nth 2 span) 223)))))

(ert-deftest poly-markdown/spans-at-point-max ()
  (pm-test-run-on-file poly-markdown-mode "markdown.md"
    (pm-goto-char (point-max))
    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) nil))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) 'tail))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) 'body))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) 'head))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) nil))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))))
