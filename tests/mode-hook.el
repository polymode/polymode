(require 'polymode-test-utils)
(require 'cl)

(ert-deftest mode-hook/runs ()
  (lexical-let (flag)
    (defcustom poly-test-mode-hook nil
      "Hook for poly-test-mode"
      :type 'hook)
    (add-hook 'poly-test-mode-hook (lambda () (setq flag t)))
    (define-polymode poly-test-mode)
    (poly-test-mode)
    (should flag)))
