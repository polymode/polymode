;; Examples of polymode configuration. Choose what suits your needs and place
;; into your .emacs file.

;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))

;;; ORG
;; org is not working presently
;; (add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))

;;; R related modes
(add-to-list 'auto-mode-alist '("\\.Snw$" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]nw$" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]md$" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.rapport$" . poly-rapport-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]html$" . poly-html+r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]brew$" . poly-brew+r-mode))
(add-to-list 'auto-mode-alist '("\\.[Rr]cpp$" . poly-r+c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp[rR]$" . poly-c++r-mode))

;;; ERB modes
(add-to-list 'auto-mode-alist '("\\.js.erb$" . poly-javascript+erb-mode))
(add-to-list 'auto-mode-alist '("\\.coffee.erb$" . poly-coffee+erb-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . poly-html+erb-mode))

;;; Slim mode
(add-to-list 'auto-mode-alist '("\\.slim$" . poly-slim-mode))

(provide 'polymode-configuration)
