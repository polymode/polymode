;;; poly-modes-tests.el -- Tests for Polymode  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; load all tests from individual poly-xyz repositories located in the parent of
;; this repository

(require 'polymode)
(require 'polymode-test-utils)


(let* ((root (expand-file-name (if (pm-test-running-on-github-p)
                                   "../polyrepos/"
                                 "../..")
                               (file-truename
                                (file-name-directory
                                 (or load-file-name buffer-file-name default-directory)))))
       (repos (directory-files root t "^poly-.*$")))
  (dolist (r repos)
    (add-to-list 'load-path r))
  (dolist (r repos)
    (let ((test-dir (expand-file-name "tests" r)))
      (when (file-exists-p test-dir)
        (let ((default-directory test-dir)
              (test-files (directory-files test-dir t "\\.el$")))
          (dolist (f test-files)
            (load f)))))))
