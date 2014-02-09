(require 'poly-base)

(defcustom pm-config/R
  (pm-config-one "R"
                 :base-submode-name 'pm-base/R
                 :inner-submode-name 'pm-submode/fundamental)
  "HTML typical configuration"
  :group 'polymode :type 'object)

;; NOWEB
(require 'poly-noweb)
(defcustom pm-config/noweb+R
  (clone pm-config/noweb
         :inner-submode-name 'pm-submode/noweb+R)
  "Noweb for R configuration"
  :group 'polymode :type 'object)

(defcustom pm-submode/noweb+R
  (clone pm-submode/noweb
         :mode 'R-mode)
  "Noweb for R"
  :group 'polymode :type 'object)

(define-polymode poly-noweb+r-mode pm-config/noweb+R)
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))



;; MARKDOWN
(require 'poly-markdown)
(define-polymode poly-markdown+r-mode pm-config/markdown :lighter " Rmd")
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))



;; RAPPORT
(defcustom pm-config/rapport
  (clone pm-config/markdown "rapport"
         :inner-submode-names '(pm-submode/brew+R
                                pm-submode/rapport+YAML))
  "Rapport template configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/rapport+YAML
  (pm-inner-submode "rapport+YAML"
                    :mode 'yaml-mode
                    :head-reg "<!--head"
                    :tail-reg "head-->")
  "YAML header in Rapport files"
  :group 'polymode  :type 'object)

(define-polymode poly-rapport-mode pm-config/rapport nil)

(add-to-list 'auto-mode-alist '("\\.rapport" . poly-rapport-mode))



;; HTML
(defcustom pm-config/html+R
  (clone pm-config/html "html+R" :inner-submode-name 'pm-submode/html+R)
  "HTML + R configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/html+R
  (pm-inner-submode "html+R"
                    :mode 'R-mode
                    :head-reg "<!--[ \t]*begin.rcode"
                    :tail-reg "end.rcode[ \t]*-->")
  "HTML KnitR submode."
  :group 'polymode  :type 'object)

(define-polymode poly-html+r-mode pm-config/html+R)
(add-to-list 'auto-mode-alist '("\\.Rhtml" . poly-html+r-mode))



;; AsciiDoc
(defcustom pm-config/doc+R
  (clone pm-config/doc "doc+R" :inner-submode-name 'pm-submode/doc+R)
  "AsciiDoc + R configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/doc+R
  (pm-inner-submode "doc+R"
                    :mode 'R-mode
                    :head-reg "//begin.rcode"
                    :tail-reg "//end.rcode")
  "AsciiDoc KnitR submode."
  :group 'polymode  :type 'object)

(define-polymode poly-doc+r-mode pm-config/doc+R)
(add-to-list 'auto-mode-alist '("\\.Rasciidoc" . poly-doc+r-mode))



;;; R-brew
(defcustom pm-config/brew+R
  (clone pm-config/brew "brew+R"
         :inner-submode-name 'pm-submode/brew+R)
  "Brew + R configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/brew+R
  (pm-inner-submode "brew+R"
                    :mode 'R-mode
                    :head-reg "<%[=%]?"
                    :tail-reg "[#=%=-]?%>")
  "Brew R chunk."
  :group 'polymode  :type 'object)

(define-polymode poly-brew+r-mode pm-config/brew+R)
(add-to-list 'auto-mode-alist '("\\.Rbrew" . poly-brew+r-mode))




;;; R+C++
;; todo: move into :matcher-subexp functionality?
(defun pm--R+C++-head-matcher (ahead)
  (when (re-search-forward "cppFunction *( *\\(['\"]\n\\)"
                           nil t ahead)
    (cons (match-beginning 1) (match-end 1))))

(defun pm--R+C++-tail-matcher (ahead)
  (when (< ahead 0)
    (goto-char (car (pm--R+C++-head-matcher -1))))
  (let ((end (or (ignore-errors (scan-sexps (point) 1))
                 (buffer-end 1))))
    (cons (max 1 (- end 1)) end)))

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
                    :font-lock-narrow nil)
  "HTML KnitR chunk."
  :group 'polymode  :type 'object)

(define-polymode poly-r+c++-mode pm-config/R+C++)
(add-to-list 'auto-mode-alist '("\\.Rcpp" . poly-r+c++-mode))



;;; C++R
(defun pm--C++R-head-matcher (ahead)
  (when (re-search-forward "^[ \t]*/[*]+[ \t]*R" nil t ahead)
    (cons (match-beginning 0) (match-end 0))))

(defun pm--C++R-tail-matcher (ahead)
  (when (< ahead 0)
    (error "backwards tail match not implemented"))
  ;; may be rely on syntactic lookup ?
  (when (re-search-forward "^[ \t]*\\*/")
    (cons (match-beginning 0) (match-end 0))))

(defcustom pm-config/C++R
  (clone pm-config/C++ "C++R" :inner-submode-name 'pm-submode/C++R)
  "R + C++ configuration"
  :group 'polymode  :type 'object)

(defcustom  pm-submode/C++R
  (pm-inner-submode "C++R"
                    :mode 'R-mode
                    :head-reg 'pm--C++R-head-matcher
                    :tail-reg 'pm--C++R-tail-matcher)
  "HTML KnitR chunk."
  :group 'polymode  :type 'object)

(define-polymode poly-c++r-mode pm-config/C++R)
(add-to-list 'auto-mode-alist '("\\.cppR" . poly-c++r-mode))



;;; R help
(defvar pm-config/ess-help+R
  (pm-config-one "ess-R-help"
                 :inner-submode-name 'pm-submode/ess-help+R)
  "ess-R-help")

(defvar  pm-submode/ess-help+R
  (pm-inner-submode "ess-help+R"
                    :mode 'R-mode
                    :head-reg "^Examples:"
                    :tail-reg "\\'"
                    :indent-offset 5)
  "Ess help R chunk")

(define-polymode poly-ess-help+r-mode pm-config/ess-help+R)
(add-hook 'ess-help-mode-hook '(lambda ()
                                 (when (string= ess-dialect "R")
                                   (poly-ess-help+r-mode))))


(defun pm--Rd-examples-head-matcher (ahead)
  (when (re-search-forward "\\examples *\\({\n\\)" nil t ahead)
    (cons (match-beginning 1) (match-end 1))))

(defun pm--Rd-examples-tail-matcher (ahead)
  (when (< ahead 0)
    (goto-char (car (pm--R+C++-head-matcher -1))))
  (let ((end (or (ignore-errors (scan-sexps (point) 1))
                 (buffer-end 1))))
    (cons (max 1 (- end 1)) end)))

(defvar pm-config/Rd
  (pm-config-one "R-documentation"
                 :inner-submode-name 'pm-submode/Rd)
  "R submode for Rd files")

(defvar  pm-submode/Rd
  (pm-inner-submode "R+C++"
                    :mode 'R-mode
                    :head-mode 'base
                    :head-reg 'pm--Rd-examples-head-matcher
                    :tail-reg 'pm--Rd-examples-tail-matcher)
  "Rd examples chunk.")

(define-polymode poly-Rd-mode pm-config/Rd)
(add-hook 'Rd-mode-hook 'poly-Rd-mode)

(provide 'poly-R)
