
;; load all tests from individual poly-xyz repositories located in the parent of
;; this repository

(require 'polymode-test)

(let* ((root (expand-file-name "../.."
                               (file-truename
                                (file-name-directory
                                 (or load-file-name buffer-file-name default-directory)))))
       (repos (directory-files root t "^poly-.*$")))
  (dolist (r repos)
    (let ((test-dir (expand-file-name "tests" r)))
      (when (file-exists-p test-dir)
        (add-to-list 'load-path r)
        (let ((default-directory test-dir)
              (test-files (directory-files test-dir t "\\.el$")))
          (dolist (f test-files)
            (load f)))))))
