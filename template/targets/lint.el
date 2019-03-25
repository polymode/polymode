
(require 'checkdoc)
(require 'elisp-lint)

(let ((sentence-end-double-space)
      (checkdoc-arguments-in-order-flag)
      (checkdoc-verb-check-experimental-flag)
      (checkdoc-force-docstrings-flag)
      (elisp-lint-indent-specs
       '((pm-test-run-on-file . 2)
         (pm-test-run-on-string . 1)
         (pm-test-poly-lock . 2)))
      (elisp-lint-ignored-validators '("package-format" "indent-character" "fill-column")))

  (elisp-lint-files-batch))
