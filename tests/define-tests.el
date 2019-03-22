;;; mode-init-tests.el --- Various initialization tests -*- lexical-binding: t -*-

(require 'polymode)
(require 'polymode-test-utils)

(ert-deftest define/hook-is-run ()
  (let (entered-hook)
    (define-polymode poly-test-mode)
    (add-hook 'poly-test-mode-hook (lambda () (setq entered-hook t)))
    (with-temp-buffer
      (poly-test-mode)
      (should entered-hook))))

(ert-deftest define/derived-mode-hook-is-run ()
  (let (hook-entered root-hook-entered mode-state root-mode-state)
    (define-polymode poly-test-root-mode)
    (define-polymode poly-test-mode poly-test-root-mode)
    (add-hook 'poly-test-root-mode-hook
              (lambda ()
                (setq hook-entered t
                      root-mode-state poly-test-root-mode)))
    (add-hook 'poly-test-mode-hook
              (lambda ()
                (setq root-hook-entered t
                      mode-state poly-test-mode)))
    (with-temp-buffer
      (poly-test-mode)
      (should hook-entered)
      (should root-hook-entered)
      (should mode-state)
      (should root-mode-state))))

(ert-deftest define/derived-mode-derives-map ()
  (define-polymode poly-test-root-mode)
  (define-polymode poly-test-mode poly-test-root-mode)
  (should (eq (keymap-parent poly-test-mode-map)
              poly-test-root-mode-map))
  (should (eq (keymap-parent (keymap-parent poly-test-mode-map))
              polymode-minor-mode-map))

  (defvar poly-xyz-polymode
    (pm-polymode :name "xyz" :keylist '(("<" . some-command))))
  (define-polymode poly-xyz+-mode poly-xyz-polymode)
  (should (eq (keymap-parent poly-xyz+-mode-map)
              polymode-minor-mode-map))
  (should (should (eq (lookup-key poly-xyz+-mode-map "<")
                      'some-command))))

(ert-deftest define/hooks-run-in-indirect-buffers ()
  (setq ran-body-in-modes nil
        ran-hooks-in-modes nil
        ran-inner-hooks-in-modes nil
        ran-host-hooks-in-modes nil)
  (define-hostmode poly-test-text-hostmode :mode 'text-mode)
  (with-slots (init-functions) poly-test-text-hostmode
    (setq init-functions '((lambda (_) (add-to-list 'ran-host-hooks-in-modes major-mode)))))
  (define-innermode poly-el-innermode
    :mode 'emacs-lisp-mode
    :head-matcher "^<<"
    :tail-matcher ">>$"
    :init-functions '((lambda (_) (add-to-list 'ran-inner-hooks-in-modes major-mode))))
  (define-polymode poly-el-mode
    :hostmode 'poly-test-text-hostmode
    :innermodes '(poly-el-innermode)
    (add-to-list 'ran-body-in-modes major-mode))
  (add-hook 'poly-el-mode-hook (lambda () (add-to-list 'ran-hooks-in-modes major-mode)))

  (pm-test-run-on-string 'poly-el-mode
    "\n<<(defvar abc nil)>>\n"
    (should (equal ran-body-in-modes '(emacs-lisp-mode poly-head-tail-mode text-mode)))
    (should (equal ran-hooks-in-modes '(emacs-lisp-mode poly-head-tail-mode text-mode)))
    (should (equal ran-inner-hooks-in-modes '(emacs-lisp-mode poly-head-tail-mode)))
    (should (equal ran-host-hooks-in-modes '(text-mode)))))

;;; mode-init-tests.el ends here
