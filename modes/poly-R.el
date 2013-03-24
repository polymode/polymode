(require 'poly-base)

;; NOWEB
(require 'poly-noweb)

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

(define-polymode poly-noweb+r-mode pm-config/noweb+R)
(add-to-list 'auto-mode-alist '("Snw" . poly-noweb+r-mode))


;; MARKDOWN
(require 'poly-markdown)
(define-polymode poly-markdown+r-mode pm-config/markdown :lighter " Rmd")
(add-to-list 'auto-mode-alist '("Rmd" . poly-markdown+r-mode))


;; HTML
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

(define-polymode poly-html+r-mode pm-config/html+R)
(add-to-list 'auto-mode-alist '("Rhtml" . poly-html+r-mode))


;;; R-brew
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

(define-polymode poly-brew+r-mode pm-config/brew+R)
(add-to-list 'auto-mode-alist '("Rbrew" . poly-brew+r-mode))


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
                    :font-lock-narrow nil
                    :protect-indent-line-function t)
  "HTML KnitR chunk."
  :group 'polymode  :type 'object)

(define-polymode poly-r+c++-mode pm-config/R+C++)
(add-to-list 'auto-mode-alist '("Rcpp" . poly-r+c++-mode))


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

(define-polymode poly-c++r-mode pm-config/C++R)
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


(define-polymode poly-ess-help+r-mode pm-config/ess-help+R)

;; will move into ESS
(defcustom ess-help-use-polymode t
  "Use polymode in ESS help when available?"
  :group 'ess
  :type 'boolean)

(when (and (boundp ess-help-use-polymode)
           ess-help-use-polymode)
  (add-hook 'ess-help-mode-hook 'poly-ess-help+r-mode))


(provide 'poly-R)
