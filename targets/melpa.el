
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
      package-deps (delq 'emacs (append dev-deps deps (mapcar #'car deps-requires)))
      package-user-dir (expand-file-name (format "ELPA/%s" emacs-version))
      ;; only melpa for speed
      package-archives '(("melpa" . "https://melpa.org/packages/")))

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(let ((refreshed nil))
  (dolist (package package-deps)
    (unless (package-installed-p package)
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install package))))
