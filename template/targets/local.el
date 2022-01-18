(load-file "targets/utils.el")

;; add .ELPA packages
(let ((elpa-dirs (directory-files (expand-file-name (format ".ELPA/%s" emacs-version)) t)))
  (setq load-path (append elpa-dirs load-path)))

;; add all poly* and deps in the parent directory and overate any in the .ELPA
(let* ((deps (cons 'poly (polymode-library-deps "__MODULE__.el")))
       (regx (format "^\\(%s\\)" (mapconcat #'symbol-name deps "\\|")))
       (local-dirs (directory-files (file-name-directory (directory-file-name default-directory))
                                    t regx)))
  (setq load-path (append local-dirs load-path)))
