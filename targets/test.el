
(setq polymode-test-dir (expand-file-name "tests/"))
(add-to-list 'load-path polymode-test-dir)
(dolist (f (directory-files polymode-test-dir t ".*el$"))
  (load f))

(toggle-debug-on-error)
(ert-run-tests-batch-and-exit t)
