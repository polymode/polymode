;;; compat-tests.el -- Tests for Polymode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.


(require 'polymode)
(require 'polymode-test-utils)
(require 'poly-markdown nil t)          ;Don't fail if not installed.

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
  (skip-unless (fboundp 'poly-markdown-mode))
  (let ((markdown-enable-math t))
    (pm-test-run-on-file poly-test-markdown-mode "test.md"
                         ;; python
                         (goto-char 44)
                         (pm-switch-to-buffer)
                         (should (equal (pm--lsp-buffer-content 108 108) ""))
                         (should (equal (pm--lsp-buffer-content 29 29) ""))
                         (should (equal (pm--lsp-buffer-content 128 128) ""))
                         (should (equal (pm--lsp-buffer-content 150 150) ""))
                         (should (equal (pm--lsp-buffer-content 151 151) ""))
                         (should (equal (pm--lsp-buffer-content 152 152) ""))

                         (should (equal (pm--lsp-buffer-content 142 143) " "))
                         (should (equal (pm--lsp-buffer-content 142 145) "   "))

                         (should (equal (pm--lsp-buffer-content 44 50) "fruits"))
                         (should (equal (pm--lsp-buffer-content 21 50)
                                        (concat (make-string 3 ?\n) "# foo\nfruits")))

                         (should (equal (pm--lsp-buffer-content 148 155)
                                        "+ x"))
                         (should (equal (pm--lsp-buffer-content 131 155)
                                        (concat (make-string 12 ? ) "   3 + x")))
                         (should (equal (pm--lsp-buffer-content 103 155)
                                        (concat "print(x)\n\n\n\n"
                                                (make-string 28 ? )
                                                "3 + x")))
                         (should (equal (pm--lsp-buffer-content 121 141)
                                        (make-string 20 ? )))
                         (should (equal (pm--lsp-buffer-content 121 131)
                                        (make-string 10 ? )))

                         ;; latex
                         (goto-char 130)
                         (pm-switch-to-buffer)
                         (should (equal (pm--lsp-buffer-content 108 108) ""))
                         (should (equal (pm--lsp-buffer-content 29 29) ""))
                         (should (equal (pm--lsp-buffer-content 128 128) ""))
                         (should (equal (pm--lsp-buffer-content 150 150) ""))
                         (should (equal (pm--lsp-buffer-content 151 151) ""))
                         (should (equal (pm--lsp-buffer-content 152 152) ""))


                         ;; markdown
                         (goto-char 21)
                         (pm-switch-to-buffer)
                         (should (equal (pm--lsp-buffer-content 108 108) ""))
                         (should (equal (pm--lsp-buffer-content 29 29) ""))
                         (should (equal (pm--lsp-buffer-content 128 128) ""))
                         (should (equal (pm--lsp-buffer-content 150 150) ""))
                         (should (equal (pm--lsp-buffer-content 151 151) ""))
                         (should (equal (pm--lsp-buffer-content 152 152) ""))

                         (should (equal (pm--lsp-buffer-content 142 143) " "))
                         (should (equal (pm--lsp-buffer-content 142 145) "   "))
                         (should (equal (pm--lsp-buffer-content 151 152) " "))

                         (should (equal (pm--lsp-buffer-content 117 121) "\nfoo"))
                         (should (equal (pm--lsp-buffer-content 118 121) "foo"))
                         (should (equal (pm--lsp-buffer-content 122 123) "$"))
                         (should (equal (pm--lsp-buffer-content 135 145) " $ bar    "))

                         (should (equal (pm--lsp-buffer-content 21 50)
                                        "some text\n\n```py\n\n"))
                         (should (equal (point) 21))

                         (let ((base (concat "\n\n```\n\nfoo $"
                                             (make-string 13 ? )
                                             "$ bar  "
                                             (make-string 8 ? )
                                             "  baz")))
                           (should (equal (pm--lsp-buffer-content 100 156)
                                          base))
                           (should (equal (pm--lsp-buffer-content 100 160)
                                          (concat base "\n```")))
                           (should (equal (pm--lsp-buffer-content 100 163)
                                          (concat base "\n```js\n")))
                           (should (equal (pm--lsp-buffer-content 100 200)
                                          (concat base "\n```js\n\n\n"))))

                         (let ((base "        baz\n```js\n\n\n"))
                           (should (equal (pm--lsp-buffer-content 145 203)
                                          base))
                           (should (equal (pm--lsp-buffer-content 136 203)
                                          (concat "$ bar    " base)))
                           (should (equal (pm--lsp-buffer-content 128 203)
                                          (concat "        $ bar    " base))))
                         (should (equal (point) 21))

                         )))



(ert-deftest compat/indent/double-poly-mode-init-preserves-original-functions ()
  (skip-unless (fboundp 'poly-markdown-mode))
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
