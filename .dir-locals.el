;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (require-final-newline . t)
  (indent-tabs-mode)
  (sentence-end-double-space . nil)
  (checkdoc-arguments-in-order-flag . nil)
  (checkdoc-force-docstrings-flag . nil)
  (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
  (bug-reference-url-format . "https://github.com/vspinu/polymode/issues/%s"))
 (emacs-lisp-mode
  (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))
