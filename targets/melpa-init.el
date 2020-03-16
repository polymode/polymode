
(require 'package)
;; to avoid puking on dir-locals on make start
(require 'bug-reference)

(setq package-user-dir (expand-file-name (format ".ELPA/%s" emacs-version))
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
