
(defun polymode-library-deps (file)
  (let ((deps (let ((file (expand-file-name "targets/deps")))
                (when (file-exists-p file)
                  (with-temp-buffer
                    (insert-file-contents file)
	                (read (buffer-string))))))
        (deps-requires (with-temp-buffer
                         (insert-file-contents file)
                         (goto-char (point-min))
                         (when (re-search-forward "Package-Requires:" nil t)
                           (car (read-from-string (buffer-substring (point) (point-at-eol))))))))
    (delq 'emacs (delete-dups (append deps (mapcar #'car deps-requires))))))

(defun polymode-add-deps-to-load-path (file)
  ;; add .ELPA packages
  (let ((elpa-dirs (directory-files (expand-file-name (format ".ELPA/%s" emacs-version)) t)))
    (setq load-path (append elpa-dirs load-path)))

  ;; add all poly* and deps in the parent directory and overate any in the .ELPA
  (let* ((deps (cons 'poly (polymode-library-deps file)))
         (regx (format "^\\(%s\\)" (mapconcat #'symbol-name deps "\\|")))
         (local-dirs (directory-files (file-name-directory (directory-file-name default-directory))
                                      t regx)))
    (setq load-path (append local-dirs load-path))))

(defun package-desc-new (name)
  (cadr (assq name package-archive-contents)))

(defun package-outdated-p (name)
  (let ((old-version (package-desc-version (cadr (assq name package-alist))))
        (new-version (package-desc-version (cadr (assq name package-archive-contents)))))
    (and (listp old-version) (listp new-version)
         (version-list-< old-version new-version))))

(defun polymode-install-packages (file)
  (require 'package)

  (setq package-user-dir (expand-file-name (format ".ELPA/%s" emacs-version))
        package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))

  (package-initialize)
  (package-refresh-contents)

  (let ((deps (polymode-library-deps file)))
    (dolist (package deps)
      (if (package-installed-p package)
          (when (package-outdated-p package)
            (package-install-from-archive (cadr (assq package package-archive-contents))))
        (package-install package)))

    (message "INSTALLED DEPS: %s"
             (mapconcat
              (lambda (pkg)
                (format "%s:%S" pkg
                        (package-desc-version (cadr (assq pkg package-archive-contents)))))
              deps
              " "))))
