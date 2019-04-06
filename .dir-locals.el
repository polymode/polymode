;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (require-final-newline . t)
  (indent-tabs-mode)
  (sentence-end-double-space . nil)
  (checkdoc-arguments-in-order-flag . nil)
  (checkdoc-force-docstrings-flag . nil)
  (checkdoc-verb-check-experimental-flag . nil)
  (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
  (bug-reference-url-format . "https://github.com/vspinu/polymode/issues/%s")
  (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))
