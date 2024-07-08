;;; poly-targets.el --- Makefile targets for maintenance of Polymode  -*- lexical-binding: t; -*-

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

;; FIXME: This file should be merged with template/targets/poly-targets.el

;;; Code:

(defmacro dlet (binders &rest body)
  "Like `let' but using dynamic scoping."
  (declare (indent 1) (debug let))
  ;; (defvar FOO) only affects the current scope, but in order for
  ;; this not to affect code after the main `let' we need to create a new scope,
  ;; which is what the surrounding `let' is for.
  ;; FIXME: (let () ...) currently doesn't actually create a new scope,
  ;; which is why we use (let (_) ...).
  `(let (_)
     ,@(mapcar (lambda (binder)
                 `(defvar ,(if (consp binder) (car binder) binder)))
               binders)
     (let ,binders ,@body)))

(defun pm--target-checkdoc ()
  (dlet ((sentence-end-double-space)
         (checkdoc-arguments-in-order-flag)
         (checkdoc-verb-check-experimental-flag)
         (checkdoc-force-docstrings-flag))
    (dolist (f command-line-args-left)
      (checkdoc-file f))))

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
            (pm-test-poly-lock . 2)
            (pm-test-spans . 1)))
         (elisp-lint-ignored-validators
          '("package-format" " indent-character" "fill-column")))

    (elisp-lint-files-batch)))

(defun pm--target-local ()
  (load-file "template/targets/utils.el")
  (polymode-add-deps-to-load-path "polymode.el"))

(defun pm--target-melpa-init ()
  (require 'package)
  ;; To avoid puking on dir-locals on make start.
  (require 'bug-reference)

  (setq package-user-dir (expand-file-name (format ".ELPA/%s" emacs-version))
        package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(defun pm--target-melpa ()
  (load "template/targets/utils.el")
  (polymode-install-packages "polymode.el"))

(defun pm--target-test ()
  (toggle-debug-on-error)

  (let ((polymode-test-dir (expand-file-name "tests/")))
    (add-to-list 'load-path polymode-test-dir)
    (dolist (f (directory-files polymode-test-dir t ".*el\\'"))
      (load f)))

  (dlet ((pm-verbose (getenv "PM_VERBOSE"))
         (ert-batch-backtrace-right-margin 200)
         (ert-batch-print-level nil)
         (ert-batch-print-length nil))

    (ert-run-tests-batch-and-exit
     (if (boundp 'pm-ert-selector)
         pm-ert-selector
       t))))

(provide 'poly-targets)
;;; poly-targets.el ends here
