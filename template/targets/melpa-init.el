
(require 'package)

(setq package-user-dir (expand-file-name (format ".ELPA/%s" emacs-version))
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
