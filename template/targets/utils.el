
(defun polymode-library-deps (lib-file)
  (let ((dev-deps '(elisp-lint))
        (deps (let ((file (expand-file-name "targets/deps")))
                (when (file-exists-p file)
                  (with-temp-buffer
                    (insert-file-contents file)
	                (read (buffer-string))))))
        (deps-requires (with-temp-buffer
                         (insert-file-contents lib-file)
                         (goto-char (point-min))
                         (when (re-search-forward "Package-Requires:" nil t)
                           (car (read-from-string (buffer-substring (point) (point-at-eol))))))))
    (delq 'emacs (delete-dups (append deps (mapcar #'car deps-requires))))))
