;;; poly-targets.el --- Makefile targets for maintenance of Polymode module  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Vitalie Spinu
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME: This file should be merged with polymode/targets/poly-targets.el

;;; Code:

(defun pm--target-checkdoc ()
  (dlet ((sentence-end-double-space)
         (checkdoc-arguments-in-order-flag)
         (checkdoc-verb-check-experimental-flag)
         (checkdoc-force-docstrings-flag))
    (let ((files (directory-files default-directory nil "^[^.].*el\\'")))
      (dolist (f files)
        (unless (member f '("__MODULE__-autoloads.el"))
          (checkdoc-file f))))))

(defun pm--target-lint ()
  (require 'checkdoc)
  (require 'elisp-lint)

  (dlet ((sentence-end-double-space)
         (checkdoc-arguments-in-order-flag)
         (checkdoc-verb-check-experimental-flag)
         (checkdoc-force-docstrings-flag)
         (elisp-lint-indent-specs
          '((pm-test-run-on-file . 2)
            (pm-test-run-on-string . 1)
            (pm-test-poly-lock . 2)))
         (elisp-lint-ignored-validators
          '("package-format" "indent-character" "fill-column")))

    (elisp-lint-files-batch)))

(defun pm--target-local ()
  (load-file "targets/utils.el")
  (polymode-add-deps-to-load-path "__MODULE__.el"))

(defun pm--target-melpa-init ()
  (require 'package)

  (setq package-user-dir (expand-file-name (format ".ELPA/%s" emacs-version))
        package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))

  (package-initialize))

(defun pm--target-melpa ()
  (load-file "targets/utils.el")
  (polymode-install-packages "__MODULE__.el"))

(defun pm--target-test ()
  (let ((polymode-test-dir (expand-file-name "tests/")))
    (add-to-list 'load-path polymode-test-dir)
    (dolist (f (directory-files polymode-test-dir t ".*el$"))
      (load f)))

  (dlet ((pm-verbose (getenv "PM_VERBOSE"))
         (ert-batch-backtrace-right-margin 2000)
         (ert-batch-print-level nil)
         (ert-batch-print-length nil)

         (debug-on-error t))

    (ert-run-tests-batch-and-exit t)))

(provide 'poly-targets)
;;; poly-targets.el ends here
