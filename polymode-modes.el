(require 'polymode)

;; BASE MODES
(defcustom pm-base/fundamental
  (pm-submode "fundamental"
              :mode 'fundamental-mode)
  "Fundamental base mode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/latex
  (pm-submode "latex"
              :mode 'latex-mode)
  "Latex base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/markdown
  (pm-submode "latex"
              :mode 'markdown-mode)
  "Markdown base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/html
  (pm-submode "html"
              :mode 'html-mode)
  "HTML base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/R
  (pm-submode "R"
              :mode 'R-mode)
  "R base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/C++
  (pm-submode "C++"
              :mode 'c++-mode)
  "C++ base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/text
  (pm-submode "text"
              :mode 'text-mode)
  "Text base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/ess-help
  (pm-submode "ess-help"
              :mode 'ess-help-mode)
  "ess-help"
  :group 'base-submodes
  :type 'object)

;; NOWEB
(defcustom pm-config/noweb
  (pm-config-one "noweb"
                 :base-submode-name 'pm-base/latex
                 :inner-submode-name 'pm-submode/noweb)
  "Noweb typical configuration"
  :group 'polymode
  :type 'object)

(defcustom  pm-submode/noweb
  (pm-inner-submode "noweb"
                    :head-reg  "^<<\\(.*\\)>>="
                    :tail-reg    "^\\(@ +%def .*\\)$\\|\\(@[ \n]\\)")
  "Noweb typical chunk."
  :group 'polymode
  :type 'object)

(defcustom pm-config/noweb+R
  (clone pm-config/noweb
         :inner-submode-name 'pm-submode/noweb+R)
  "Noweb for R configuration"
  :group 'polymode
  :type 'object)

(defcustom pm-submode/noweb+R
  (clone pm-submode/noweb
         :mode 'R-mode
         :protect-indent-line-function t)
  "Noweb for R"
  :group 'polymode
  :type 'object)

(define-derived-mode poly-noweb+r-mode fundamental-mode "Noweb+R"
  "Mode for editing noweb documents.
Supports differnt major modes for doc and code chunks using multi-mode."
  (pm/initialize (clone pm-config/noweb-R)))

(add-to-list 'auto-mode-alist '("Snw" . poly-noweb+r-mode))

;;; MARKDOWN
(defcustom pm-config/markdown
  (pm-config-multi-auto "markdown"
                        :base-submode-name 'pm-base/markdown
                        :auto-submode-name 'pm-submode/markdown)
  "Markdown typical configuration"
  :group 'polymode
  :type 'object)

(defcustom  pm-submode/markdown
  (pm-inner-submode-auto "markdown"
                         :head-reg "^[ \t]*```[{ \t]*\\w.*$"
                         :tail-reg "^[ \t]*```[ \t]*$"
                         :retriever-regexp "```[ \t]*{?\\(\\w+\\)")
  "Noweb typical chunk."
  :group 'polymode
  :type 'object)


(define-derived-mode poly-markdown+R-mode fundamental-mode "Rmd"
  "Mode for editing noweb documents.
Supports differnt major modes for doc and code chunks using multi-mode."
  (pm/initialize (clone pm-config/markdown)))

(define-minor-mode poly-markdown+R-minor-mode
  "Polymode minor mode, used to make everything work."
  nil " Rmd" polymode-mode-map
  (if Rmd-minor-mode
      (unless pm/config
        (let ((config (clone pm-config/markdown)))
          (oset config :minor-mode-name 'Rmd-minor-mode)
          (pm/initialize config)))
    (setq pm/config nil
          pm/submode nil)))

(add-to-list 'auto-mode-alist '("Rmd" . poly-markdown-R-mode))


;;; HTML
(defcustom pm-config/html
  (pm-config-one "html"
                 :base-submode-name 'pm-base/html
                 :inner-submode-name 'pm-submode/fundamental)
  "HTML typical configuration"
  :group 'polymode :type 'object)

(defcustom pm-config/html+R
  (clone pm-config/html "html+R" :inner-submode-name 'pm-submode/html+R)
  "HTML + R configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/html+R
  (pm-inner-submode "html+R"
                    :mode 'R-mode
                    :head-reg "<!--[ \t]*begin.rcode"
                    :tail-reg "end.rcode[ \t]*-->"
                    :protect-indent-line-function t)
  "HTML KnitR chunk."
  :group 'polymode  :type 'object)


(define-derived-mode poly-html+r-mode fundamental-mode "html+R"
  "Polymode for html + R"
  (pm/initialize (clone pm-config/html+R)))

(add-to-list 'auto-mode-alist '("Rhtml" . poly-html+r-mode))

;;; R-brew
(defcustom pm-config/brew
  (pm-config-one "brew"
                 :base-submode-name 'pm-base/text
                 :inner-submode-name 'pm-submode/fundamental)
  "HTML typical configuration"
  :group 'polymode :type 'object)

(defcustom pm-config/brew+R
  (clone pm-config/brew "brew+R" :inner-submode-name 'pm-submode/brew+R)
  "Brew + R configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/brew+R
  (pm-inner-submode "brew+R"
                    :mode 'R-mode
                    :head-reg "<%[=%]?"
                    :tail-reg "[#=%=-]?%>"
                    :protect-indent-line-function t)
  "Brew R chunk."
  :group 'polymode  :type 'object)


(define-derived-mode poly-brew+r-mode fundamental-mode "brew+R"
  "Polymode for brew + R"
  (pm/initialize (clone pm-config/brew+R)))

(add-to-list 'auto-mode-alist '("Rbrew" . poly-brew+r-mode))

;;; R+C++
;; todo: move into :matcher-subexp functionality?
(defun pm--R+C++-head-matcher (ahead)
  (when (re-search-forward "cppFunction *( *\\(['\"]\\)"
                           nil t ahead)
    (cons (match-beginning 1) (match-end 1))))

;; (defvar pm--C++R-syntax-table (make-syntax-table))
;; (modify-syntax-entry ?\' "\"" pm--C++R-syntax-table)
;; (modify-syntax-entry ?\" "\"" pm--C++R-syntax-table)

(defun pm--R+C++-tail-matcher (ahead)
  (when (< ahead 0)
    (goto-char (car (pm--R+C++-head-matcher -1))))
  ;; (with-syntax-table pm--C++R-syntax-table
    (let ((end (or (ignore-errors (scan-sexps (point) 1))
                   (buffer-end 1))))
      (cons (max 1 (- end 1)) end)))

(defcustom pm-config/R
  (pm-config-one "R"
                 :base-submode-name 'pm-base/R
                 :inner-submode-name 'pm-submode/fundamental)
  "HTML typical configuration"
  :group 'polymode :type 'object)

(defcustom pm-config/R+C++
  (clone pm-config/R "R+C++" :inner-submode-name 'pm-submode/R+C++)
  "R + C++ configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/R+C++
  (pm-inner-submode "R+C++"
                    :mode 'c++-mode
                    :head-mode 'base
                    :head-reg 'pm--R+C++-head-matcher
                    :tail-reg 'pm--R+C++-tail-matcher
                    :protect-indent-line-function t)
  "HTML KnitR chunk."
  :group 'polymode  :type 'object)

(define-derived-mode poly-r+c++-mode fundamental-mode "R+C++"
  "Polymode for R+C++"
  (pm/initialize (clone pm-config/R+C++)))

(add-to-list 'auto-mode-alist '("Rcpp" . poly-r+c++-mode))

;;; C++R
(defun pm--C++R-head-matcher (ahead)
  (when (re-search-forward "^[ \t]*/[*]+[ \t]*R" nil t ahead)
    (cons (match-beginning 0) (match-end 0))))

(defun pm--C++R-tail-matcher (ahead)
  (when (< ahead 0)
    (error "backwards tail match not implemented"))
  ;; todo: may be base it on syntactic lookup 
  (when (re-search-forward "^[ \t]*\\*/")
    (cons (match-beginning 0) (match-end 0))))

(defcustom pm-config/C++
  (pm-config-one "C++"
                 :base-submode-name 'pm-base/C++
                 :inner-submode-name 'pm-submode/fundamental)
  "C++ typical configuration"
  :group 'polymode :type 'object)

(defcustom pm-config/C++R
  (clone pm-config/C++ "C++R" :inner-submode-name 'pm-submode/C++R)
  "R + C++ configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/C++R
  (pm-inner-submode "C++R"
                    :mode 'R-mode
                    :background 1.1
                    :head-reg 'pm--C++R-head-matcher
                    :tail-reg 'pm--C++R-tail-matcher
                    :protect-indent-line-function t)
  "HTML KnitR chunk."
  :group 'polymode  :type 'object)

(define-derived-mode poly-c++r-mode fundamental-mode "C++R"
  "Polymode for C++R"
  (pm/initialize (clone pm-config/C++R)))

(add-to-list 'auto-mode-alist '("cppR" . poly-c++r-mode))


;;; R help
(defcustom pm-config/ess-help+R
  (pm-config-one "ess-R-help"
                 :base-submode-name 'pm-base/ess-help
                 :inner-submode-name 'pm-submode/ess-help+R)
  "ess-R-help"
  :group 'polymode :type 'object)

(defcustom  pm-submode/ess-help+R
  (pm-inner-submode "ess-help+R"
                    :mode 'R-mode
                    :head-reg "^Examples:"
                    :tail-reg "\\'"
                    :protect-indent-line-function t)
  "Ess help R chunk"
  :group 'polymode  :type 'object)

(define-minor-mode poly-ess-help+r-minor-mode
  "ess help"
  nil " RPM" polymode-mode-map
  (if poly-ess-help+R-minor-mode
      (unless pm/config
        (let ((config (clone pm-config/ess-help+R)))
          (oset config :minor-mode-name 'poly-ess-help+R-minor-mode)
          (pm/initialize config)))
    (setq pm/config nil
          pm/submode nil)))

(defcustom ess-help-use-polymode t
  "Use polymode in ESS help when available?"
  :group 'ess
  :type 'boolean)

(when (and (boundp ess-help-use-polymode)
           ess-help-use-polymode)
  (add-hook 'ess-help-mode-hook 'poly-ess-help+r-minor-mode))
    

(provide 'polymode-modes)
