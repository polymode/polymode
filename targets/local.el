
;; add all poly* directories from the parent directory to load path
(let ((poly-dirs (directory-files (file-name-directory (directory-file-name default-directory))
                                  t "^poly")))
  (setq load-path (append poly-dirs load-path)))
