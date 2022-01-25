(require 'poly-markdown)

(define-innermode poly-test-markdown-inline-python-innermode  poly-markdown-inline-code-innermode
  :head-matcher (cons "[^`]\\(`py \\)" 1)
  :mode 'python-mode
  :fallback-mode 'poly-fallback-mode
  :tail-mode nil
  :head-mode nil)

(define-polymode poly-test-markdown-mode poly-markdown-mode
  :innermodes `(:inherit
                poly-test-markdown-inline-python-innermode))

(ert-deftest compat/lsp/lsp-text ()
  (let ((markdown-enable-math t))
    (pm-test-run-on-file poly-test-markdown-mode "test.md"
      ;; python
      (goto-char 44)
      (pm-switch-to-buffer)
      (should (equal (pm--lsp-text 108 108) ""))
      (should (equal (pm--lsp-text 29 29) ""))
      (should (equal (pm--lsp-text 128 128) ""))
      (should (equal (pm--lsp-text 150 150) ""))
      (should (equal (pm--lsp-text 151 151) ""))
      (should (equal (pm--lsp-text 152 152) ""))

      (should (equal (pm--lsp-text 142 143) " "))
      (should (equal (pm--lsp-text 142 145) "   "))

      (should (equal (pm--lsp-text 44 50) "fruits"))
      (should (equal (pm--lsp-text 21 50)
                     (concat (make-string 3 ?\n) "# foo\nfruits")))

      (should (equal (pm--lsp-text 148 155)
                     "+ x"))
      (should (equal (pm--lsp-text 131 155)
                     (concat (make-string 12 ? ) "   3 + x")))
      (should (equal (pm--lsp-text 103 155)
                     (concat "print(x)\n\n\n\n"
                             (make-string 28 ? )
                             "3 + x")))
      (should (equal (pm--lsp-text 121 141)
                     (make-string 20 ? )))
      (should (equal (pm--lsp-text 121 131)
                     (make-string 10 ? )))

      ;; latex
      (goto-char 130)
      (pm-switch-to-buffer)
      (should (equal (pm--lsp-text 108 108) ""))
      (should (equal (pm--lsp-text 29 29) ""))
      (should (equal (pm--lsp-text 128 128) ""))
      (should (equal (pm--lsp-text 150 150) ""))
      (should (equal (pm--lsp-text 151 151) ""))
      (should (equal (pm--lsp-text 152 152) ""))


      ;; markdown
      (goto-char 21)
      (pm-switch-to-buffer)
      (should (equal (pm--lsp-text 108 108) ""))
      (should (equal (pm--lsp-text 29 29) ""))
      (should (equal (pm--lsp-text 128 128) ""))
      (should (equal (pm--lsp-text 150 150) ""))
      (should (equal (pm--lsp-text 151 151) ""))
      (should (equal (pm--lsp-text 152 152) ""))

      (should (equal (pm--lsp-text 142 143) " "))
      (should (equal (pm--lsp-text 142 145) "   "))
      (should (equal (pm--lsp-text 151 152) " "))

      (should (equal (pm--lsp-text 117 121) "\nfoo"))
      (should (equal (pm--lsp-text 118 121) "foo"))
      (should (equal (pm--lsp-text 122 123) "$"))
      (should (equal (pm--lsp-text 135 145) " $ bar    "))

      (should (equal (pm--lsp-text 21 50)
                     "some text\n\n```py\n\n"))
      (should (equal (point) 21))

      (let ((base (concat "\n\n```\n\nfoo $"
                          (make-string 13 ? )
                          "$ bar  "
                          (make-string 8 ? )
                          "  baz")))
        (should (equal (pm--lsp-text 100 156)
                       base))
        (should (equal (pm--lsp-text 100 160)
                       (concat base "\n```")))
        (should (equal (pm--lsp-text 100 163)
                       (concat base "\n```js\n")))
        (should (equal (pm--lsp-text 100 200)
                       (concat base "\n```js\n\n\n"))))

      (let ((base "        baz\n```js\n\n\n"))
        (should (equal (pm--lsp-text 145 203)
                       base))
        (should (equal (pm--lsp-text 136 203)
                       (concat "$ bar    " base)))
        (should (equal (pm--lsp-text 128 203)
                       (concat "        $ bar    " base))))
      (should (equal (point) 21))

      )))



(ert-deftest compat/indent/double-poly-mode-init-preserves-original-functions ()
  (pm-test-run-on-file poly-test-markdown-mode "test.md"
    (goto-char (point-min))
    (pm-switch-to-buffer)
    (should (eq major-mode 'markdown-mode))
    (poly-test-markdown-mode t)
    (should (equal pm--indent-line-function-original 'markdown-indent-line))
    (should (equal pm--indent-region-function-original 'markdown--indent-region))
    (should (equal pm--fill-forward-paragraph-original 'markdown-fill-forward-paragraph))
    (should (equal pm--syntax-propertize-function-original 'markdown-syntax-propertize))

    (re-search-forward "http.createServer")
    (forward-line 1)
    (pm-switch-to-buffer)
    (poly-test-markdown-mode t)
    (should (eq major-mode 'js-mode))
    (should (equal pm--indent-line-function-original 'js-indent-line))
    (should (equal pm--indent-region-function-original 'pm--indent-region-line-by-line))
    (should (equal pm--fill-forward-paragraph-original 'forward-paragraph))))
