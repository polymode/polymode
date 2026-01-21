;; -*- mode: eask; lexical-binding: t -*-

(package "polymode"
         "0.2.2"
         "Extensible framework for multiple major modes")

(website-url "https://github.com/polymode/polymode")
(keywords "languages" "multi-modes" "processes")

(package-file "polymode.el")
(files "polymode-*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "25")

(development
 (depends-on "ess")
 (depends-on "markdown-mode")
 (depends-on "yaml-mode")
 (depends-on "rust-mode")
 (depends-on "slim-mode")
 (depends-on "elisp-lint")
 (depends-on "coffee-mode")
 (depends-on "poly-markdown"))

;; For byte-compile

(add-hook 'eask-before-compile-hook
           (lambda ()
             ;; Turn this on for more restriction.
             ;;(setq byte-compile-error-on-warn t)
             ))

;; For ERT tests

(load-paths (expand-file-name "tests/"))

(add-hook 'eask-before-test/ert-hook
           (lambda ()
             (setq ert-batch-backtrace-right-margin 200)
             (setq ert-batch-print-level nil)
             (setq ert-batch-print-length nil)))
