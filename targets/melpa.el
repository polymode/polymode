
(require 'package)

(setq dev-deps '(elisp-lint)
      deps (let ((file (expand-file-name "targets/deps")))
             (when (file-exists-p file)
               (with-temp-buffer
                 (insert-file-contents file)
	             (read (buffer-string)))))
      deps-requires (with-temp-buffer
                      (insert-file-contents "polymode.el")
                      (goto-char (point-min))
                      (when (re-search-forward "Package-Requires:" nil t)
                        (car (read-from-string (buffer-substring (point) (point-at-eol))))))
      package-deps (delq 'emacs (delete-dups (append deps (mapcar #'car deps-requires))))
      package-user-dir (expand-file-name (format ".ELPA/%s" emacs-version))
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(defun package-desc-new (name)
  (cadr (assq name package-archive-contents)))

(defun package-outdated-p (name)
  (let ((old-version (package-desc-version (cadr (assq name package-alist))))
        (new-version (package-desc-version (cadr (assq name package-archive-contents)))))
    (and (listp old-version) (listp new-version)
         (version-list-< old-version new-version))))

(package-initialize)
(package-refresh-contents)

(dolist (package package-deps)
  (if (package-installed-p package)
      (when (package-outdated-p package)
        (package-install-from-archive (cadr (assq package package-archive-contents))))
    (package-install package)))

(message "INSTALLED DEPS: %s"
         (mapconcat
          (lambda (pkg)
            (format "%s:%S" pkg
                    (package-desc-version (cadr (assq pkg package-archive-contents)))))
          package-deps
          " "))
