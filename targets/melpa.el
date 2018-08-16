
(require 'package)

(setq package-deps '(markdown-mode
                     coffee-mode markdown-mode slim-mode)
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
