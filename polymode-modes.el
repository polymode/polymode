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

(defcustom pm-base/text
  (pm-submode "text"
              :mode 'text-mode)
  "Text base submode"
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

(define-derived-mode poly-noweb+R-mode fundamental-mode "Noweb+R"
  "Mode for editing noweb documents.
Supports differnt major modes for doc and code chunks using multi-mode."
  (pm/initialize (clone pm-config/noweb-R)))

(add-to-list 'interpreter-mode-alist '("noweb+R" . poly-noweb+R-mode))
(add-to-list 'auto-mode-alist '("Snw" . poly-noweb+R-mode))


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


(define-derived-mode poly-html+R-mode fundamental-mode "html+R"
  "Polymode for html + R"
  (pm/initialize (clone pm-config/html+R)))

(add-to-list 'auto-mode-alist '("Rhtml" . poly-html+R-mode))

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


(define-derived-mode poly-brew+R-mode fundamental-mode "brew+R"
  "Polymode for brew + R"
  (pm/initialize (clone pm-config/brew+R)))

(add-to-list 'auto-mode-alist '("Rbrew" . poly-brew+R-mode))

;;; R+C++
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


;; todo: move into :matcher-subexp functionality?
(defun pm--R+C++-head-matcher (ahead)
  (when (re-search-forward "cppFunction[ \t]*(\\(['\"]\\)"
                           nil t ahead)
    (cons (match-beginning 1) (match-end 1))))

(defun pm--R+C++-tail-matcher (ahead)
  (when (< ahead 0)
    (goto-char (car (pm--R+C++-head-matcher -1))))
  (let ((end (condition-case err
                 (progn (forward-sexp)
                        (point))
               (error (point-max)))))
    (cons (- end 1) end)))
 
(defcustom  pm-submode/R+C++
  (pm-inner-submode "R+C++"
                    :mode 'c++-mode
                    :head-reg 'pm--R+C++-head-matcher
                    :tail-reg 'pm--R+C++-tail-matcher
                    :protect-indent-line-function t)
  "HTML KnitR chunk."
  :group 'polymode  :type 'object)

(define-derived-mode poly-R+C++-mode fundamental-mode "R+C++"
  "Polymode for R+C++"
  (pm/initialize (clone pm-config/R+C++)))

(add-to-list 'interpreter-mode-alist '("R+C++" . poly-R+C++-mode))
(add-to-list 'auto-mode-alist '("Rcpp" . poly-R+C++-mode))


;;; C++R
(defcustom pm-config/C++
  (pm-config-one "C++"
                 :base-submode-name 'pm-base/C++
                 :inner-submode-name 'pm-submode/fundamental)
  "C++ typical configuration"
  :group 'polymode :type 'object)

(defcustom pm-config/C++R
  (clone pm-config/R "C++R" :inner-submode-name 'pm-submode/C++R)
  "R + C++ configuration"
  :group 'polymode  :type 'object)


;; todo: move into :matcher-subexp functionality?
(defun pm--C++R-head-matcher (ahead)
  (when (re-search-forward "^[ \t]*/[*]+[ \t]*R" nil t ahead)
    (cons (match-beginning 0) (match-end 0))))

(defun pm--C++R-tail-matcher (ahead)
  (when (< ahead 0)
    (error "backwards tail match not implemented"))
  (when (re-search-forward "^[ \t]*\\*/")
    (cons (match-beginning 0) (match-end 0))))

(defcustom  pm-submode/C++R
  (pm-inner-submode "C++R"
                    :mode 'fundamental-mode
                    :head-reg 'pm--C++R-head-matcher
                    :tail-reg 'pm--C++R-tail-matcher
                    :protect-indent-line-function t)
  "HTML KnitR chunk."
  :group 'polymode  :type 'object)

(define-derived-mode poly-C++R-mode fundamental-mode "C++R"
  "Polymode for C++R"
  (pm/initialize (clone pm-config/C++R)))

(add-to-list 'interpreter-mode-alist '("C++R" . poly-C++R-mode))
(add-to-list 'auto-mode-alist '("cppR" . poly-C++R-mode))



;; (defcustom pm-submode/markdown-R
;;   (clone pm-submode/markdown "markdown-R"
;;          :mode 'R-mode
;;          :protect-indent-line-function t)
;;   "Markdown for R"
;;   :group 'polymode
;;   :type 'object)


(provide 'polymode-modes)
