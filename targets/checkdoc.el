
(let ((sentence-end-double-space)
      (checkdoc-arguments-in-order-flag)
      (checkdoc-verb-check-experimental-flag)
      (checkdoc-force-docstrings-flag))
  (let ((files (directory-files default-directory t ".*el$")))
    (dolist (f files)
      (checkdoc-file f))))
