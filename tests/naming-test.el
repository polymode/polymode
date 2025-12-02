;;; naming-tests.el -- Tests for Polymode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

(require 'polymode-test-utils)

(define-innermode basic-lisp-code-innermode
        :mode 'lisp-mode
        :head-matcher "^---s\n"
        :tail-matcher "^---e\n"
        :head-mode 'host
        :tail-mode 'host)

(define-polymode poly-basic-text-mode
        :hostmode 'poly-text-hostmode
        :innermodes '(basic-lisp-code-innermode))

(ert-deftest naming/buffer ()
        (pm-test-run-on-string 'poly-basic-text-mode
                "this is a
basic test
---s
(+ 1 2)
(cons 'sym nil)
---e
meow." 1
                (let ((old-host-name (buffer-name))
          (old-inner-name))
      (pm-switch-to-buffer)
      (should (string-equal old-host-name
                            (buffer-name)))

      (goto-line 4)
      (pm-switch-to-buffer)
      (setq old-inner-name (buffer-name))
      (pm-switch-to-buffer)
      (should (string-equal old-inner-name
                            (buffer-name)))

      (goto-line 1)
      (pm-switch-to-buffer)
      (should (string-equal old-host-name
                            (buffer-name))))))
