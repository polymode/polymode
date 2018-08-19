
(require 'package)


(setq deps (let ((file (expand-file-name "targets/deps")))
             (when (file-exists-p file)
               (with-temp-buffer
                 (insert-file-contents file)
	             (read (buffer-string))))))

(setq deps-requires (with-temp-buffer
                      (insert-file-contents "__MODULE__.el")
                      (goto-char (point-min))
                      (when (re-search-forward "Package-Requires:" nil t)
                        (car (read-from-string (buffer-substring (point) (point-at-eol)))))))

(setq
 package-deps (delq 'emacs (delete-dups (append deps (mapcar #'car deps-requires))))
 package-user-dir (expand-file-name (format "ELPA/%s" emacs-version)))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(let ((refreshed nil))
  (dolist (package package-deps)
    (unless (package-installed-p package)
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install package))))
