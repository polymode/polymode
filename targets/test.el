(toggle-debug-on-error)

(setq polymode-test-dir (expand-file-name "tests/"))
(add-to-list 'load-path polymode-test-dir)
(dolist (f (directory-files polymode-test-dir t ".*el$"))
  (load f))

(setq pm-verbose (getenv "PM_VERBOSE")
      ert-batch-backtrace-right-margin 200
      ert-batch-print-level nil
      ert-batch-print-length nil)

(ert-run-tests-batch-and-exit
 (if (boundp 'pm-ert-selector)
     pm-ert-selector
   t))
