
(require 'polymode-test)

(ert-deftest poly-markdown/headings ()
  (pm-test-file poly-markdown-mode "markdown.md"
    (insert-1
     (pm-goto-char 61)
     (insert " ")
     (pm-test-chunks)
     (delete-backward-char 1))
    (delete-2
     (pm-goto-char 1828)
     (end-of-line 1)
     (backward-kill-word 1))
    (insert-new-line-3
     (pm-goto-char 2142)
     (insert "\n")
     (pm-test-chunks)
     (delete-backward-char 1))))

(ert-deftest poly-markdown/fenced-code ()
  (pm-test-file poly-markdown-mode "markdown.md"
    (delete-fortran-print
     (pm-goto-char 516)
     (delete-backward-char 1))
    (insert-ada-hello
     (pm-goto-char 1106)
     (undo-boundary)
     (insert "\"hello!\"\n")
     (undo-boundary))
    (insert-lisp-arg
     (pm-goto-char 1421)
     (insert "first-arg "))
    (python-kill-line
     (pm-goto-char 2779)
     (beginning-of-line 1)
     (kill-line 3))
    (elisp-kill-sexp
     (pm-goto-char 3534)
     (kill-sexp))
    (elisp-kill-defun
     (pm-goto-char 3119)
     (kill-sexp))))
