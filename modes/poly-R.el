;;; poly-R.el --- Popymodes for R
;;
;; Filename: poly-R.el
;; Author: Spinu Vitalie
;; Maintainer: Spinu Vitalie
;; Copyright (C) 2013-2014, Spinu Vitalie, all rights reserved.
;; Version: 1.0
;; URL: https://github.com/vitoshka/polymode
;; Keywords: emacs
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'polymode)

(defcustom pm-config/R
  (pm-config-one "R"
                 :basemode 'pm-base/R
                 :chunkmode 'pm-chunk/fundamental)
  "HTML typical configuration"
  :group 'polymode-configs
  :type 'object)

;; NOWEB
(require 'poly-noweb)
(defcustom pm-config/noweb+R
  (clone pm-config/noweb
         :chunkmode 'pm-chunk/noweb+R)
  "Noweb for R configuration"
  :group 'polymode-configs
  :type 'object)

(defcustom pm-chunk/noweb+R
  (clone pm-chunk/noweb
         :mode 'R-mode)
  "Noweb for R"
  :group 'polymode-chunkmodes
  :type 'object)

;;;###autoload (autoload 'poly-noweb+r-mode "poly-R")
(define-polymode poly-noweb+r-mode pm-config/noweb+R)



;; MARKDOWN
(require 'poly-markdown)
;;;###autoload (autoload 'poly-markdown+r-mode "poly-R")
(define-polymode poly-markdown+r-mode pm-config/markdown :lighter " Rmd")



;; RAPPORT
(defcustom pm-config/rapport
  (clone pm-config/markdown "rapport"
         :chunkmodes '(pm-chunk/brew+R
                       pm-chunk/rapport+YAML))
  "Rapport template configuration"
  :group 'polymode-configs
  :type 'object)

(defcustom  pm-chunk/rapport+YAML
  (pm-chunkmode "rapport+YAML"
                :mode 'yaml-mode
                :head-reg "<!--head"
                :tail-reg "head-->")
  "YAML header in Rapport files"
  :group 'polymode-chunkmodes
  :type 'object)



;;;###autoload (autoload 'poly-rapport-mode "poly-R")
(define-polymode poly-rapport-mode pm-config/rapport nil)



;; HTML
(defcustom pm-config/html+R
  (clone pm-config/html "html+R" :chunkmode 'pm-chunk/html+R)
  "HTML + R configuration"
  :group 'polymode-configs
  :type 'object)

(defcustom  pm-chunk/html+R
  (pm-chunkmode "html+R"
                :mode 'R-mode
                :head-reg "<!--[ \t]*begin.rcode"
                :tail-reg "end.rcode[ \t]*-->")
  "HTML KnitR submode."
  :group 'polymode-chunkmodes
  :type 'object)

;;;###autoload (autoload 'poly-html+r-mode "poly-R")
(define-polymode poly-html+r-mode pm-config/html+R)



;;; R-brew
(defcustom pm-config/brew+R
  (clone pm-config/brew "brew+R"
         :chunkmode 'pm-chunk/brew+R)
  "Brew + R configuration"
  :group 'polymode-configs
  :type 'object)

(defcustom  pm-chunk/brew+R
  (pm-chunkmode "brew+R"
                :mode 'R-mode
                :head-reg "<%[=%]?"
                :tail-reg "[#=%=-]?%>")
  "Brew R chunk."
  :group 'polymode-chunkmodes
  :type 'object)

;;;###autoload (autoload 'poly-brew+r-mode "poly-R")
(define-polymode poly-brew+r-mode pm-config/brew+R)



;;; R+C++
;; todo: move into :matcher-subexp functionality?
(defun pm--R+C++-head-matcher (ahead)
  (when (re-search-forward "cppFunction(\\(['\"]\n\\)"
                           nil t ahead)
    (cons (match-beginning 1) (match-end 1))))

(defun pm--R+C++-tail-matcher (ahead)
  (when (< ahead 0)
    (goto-char (car (pm--R+C++-head-matcher -1))))
  (goto-char (max 1 (1- (point))))
  (let ((end (or (ignore-errors (scan-sexps (point) 1))
                 (buffer-end 1))))
    (cons (max 1 (1- end)) end)))

(defcustom pm-config/R+C++
  (clone pm-config/R "R+C++" :chunkmode 'pm-chunk/R+C++)
  "R + C++ configuration"
  :group 'polymode-configs
  :type 'object)

(defcustom  pm-chunk/R+C++
  (pm-chunkmode "R+C++"
                :mode 'c++-mode
                :head-mode 'base
                :head-reg 'pm--R+C++-head-matcher
                :tail-reg 'pm--R+C++-tail-matcher
                :font-lock-narrow nil)
  "HTML KnitR chunk."
  :group 'polymode-chunkmodes
  :type 'object)

;;;###autoload (autoload 'poly-r+c++-mode "poly-R")
(define-polymode poly-r+c++-mode pm-config/R+C++)



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
  (clone pm-config/C++ "C++R" :chunkmode 'pm-chunk/C++R)
  "R + C++ configuration"
  :group 'polymode-configs
  :type 'object)

(defcustom  pm-chunk/C++R
  (pm-chunkmode "C++R"
                :mode 'R-mode
                :head-reg 'pm--C++R-head-matcher
                :tail-reg 'pm--C++R-tail-matcher)
  "HTML KnitR chunk."
  :group 'polymode-configs
  :type 'object)

;;;###autoload (autoload 'poly-c++r-mode "poly-R")
(define-polymode poly-c++r-mode pm-config/C++R)



;;; R help
(defcustom pm-config/ess-help+R
  (pm-config-one "ess-R-help"
                 :chunkmode 'pm-chunk/ess-help+R)
  "ess-R-help"
  :group 'polymode-configs
  :type 'object)

(defcustom  pm-chunk/ess-help+R
  (pm-chunkmode "ess-help+R"
                :mode 'R-mode
                :head-reg "^Examples:"
                :tail-reg "\\'"
                :indent-offset 5)
  "Ess help R chunk"
  :group 'polymode-chunkmodes
  :type 'object)

;;;###autoload (autoload 'poly-ess-help+r-mode "poly-R")
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

(defcustom pm-config/Rd
  (pm-config-one "R-documentation"
                 :chunkmode 'pm-chunk/Rd)
  "R submode for Rd files"
  :group 'polymode-configs
  :type 'object)

(defcustom pm-chunk/Rd
  (pm-chunkmode "R+C++"
                :mode 'R-mode
                :head-mode 'base
                :head-reg 'pm--Rd-examples-head-matcher
                :tail-reg 'pm--Rd-examples-tail-matcher)
  "Rd examples chunk."
  :group 'polymode-chunkmodes
  :type 'object)

;;;###autoload (autoload 'poly-Rd-mode "poly-R")
(define-polymode poly-Rd-mode pm-config/Rd)
(add-hook 'Rd-mode-hook 'poly-Rd-mode)



;; R SHELL WEAVERS
(defcustom pm-weaver/knitR
  (pm-shell-weaver "knitr"
            :from-to
            '(("latex" "\\.\\(tex\\|rnw\\)\\'" "tex" "LaTeX" "Rscript -e \"library(knitr); knit('%i', output='%o')\"")
              ("html" "\\.x?html?\\'" "html" "HTML" "Rscript -e \"library(knitr); knit('%i', output='%o')\"")
              ("markdown" "\\.r?md\\'" "md" "Markdown" "Rscript -e \"library(knitr); knit('%i', output='%o')\"")
              ("rst" "\\.rst" "rst" "ReStructuredText" "Rscript -e \"library(knitr); knit('%i', output='%o')\"")
              ("brew" "\\.r?brew\\'" "brew" "Brew" "Rscript -e \"library(knitr); knit('%i', output='%o')\"")
              ("asciidoc" "\\.asciidoc\\'" "txt" "AsciiDoc" "Rscript -e \"library(knitr); knit('%i', output='%o')\"")
              ("textile" "\\.textile\\'" "textile" "Textile" "Rscript -e \"library(knitr); knit('%i', output='%o')\"")))
  "Shell knitR weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/knitR nil
                          pm-config/noweb+R pm-config/markdown
                          pm-config/rapport pm-config/html+R)

(defcustom pm-weaver/Sweave
  (pm-shell-weaver "sweave"
                  :from-to
                  '(("latex" "\\.\\(tex\\|r?s?nw\\)\\'"
                     "tex" "LaTeX" "R CMD Sweave %i --options=\"output='%o'\"")))
  "Shell 'Sweave' weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/Sweave nil
                          pm-config/noweb+R)

;; (oref pm-config/rapport :weavers)
;; (oref pm-config/noweb+R :weavers)



;; R ESS WEAVERS
(defcustom pm-weaver/knitR-ESS
  (pm-callback-weaver "knitR-ESS"
                      :from-to
                      '(("latex" "\\.\\(tex\\|rnw\\)\\'" "tex" "LaTeX" "library(knitr); knit('%i', output='%o')")
                        ("html" "\\.x?html?\\'" "html" "HTML" "library(knitr); knit('%i', output='%o')")
                        ("markdown" "\\.r?md\\'" "md" "Markdown" "library(knitr); knit('%i', output='%o')")
                        ("rst" "\\.rst\\'" "rst" "ReStructuredText" "library(knitr); knit('%i', output='%o')")
                        ("brew" "\\.r?brew\\'" "brew" "Brew" "library(knitr); knit('%i', output='%o')")
                        ("asciidoc" "\\.asciidoc\\'" "txt" "AsciiDoc" "library(knitr); knit('%i', output='%o')")
                        ("textile" "\\.textile\\'" "textile" "Textile" "library(knitr); knit('%i', output='%o')"))
                      :function 'pm--run-command-in-ESS
                      :callback 'pm--ESS-callback)
  "ESS knitR weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/knitR-ESS nil
                          pm-config/noweb+R pm-config/markdown
                          pm-config/rapport pm-config/html+R)

(defcustom pm-weaver/Sweave-ESS
  (pm-callback-weaver "ESS-Sweave"
                      :from-to '(("latex" "\\.\\(tex\\|r?s?nw\\)\\'" "tex"
                                  "LaTeX" "Sweave('%i', output='%o')"))
             :function 'pm--run-command-in-ESS
             :callback 'pm--ESS-callback)
  "ESS 'Sweave' weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/Sweave-ESS nil
                          pm-config/noweb+R)

(declare-function ess-async-command nil)
(declare-function ess-force-buffer-current nil)
(declare-function ess-process-get nil)
(declare-function ess-process-put nil)
(declare-function comint-next-prompt nil)

(defun pm--ESS-callback (proc string)
  (let ((ofile (process-get proc 'pm-output-file)))
   (with-current-buffer (process-buffer proc)
     (when (string-match-p "Error\\(:\\| +in\\)" string)
       (error "Errors durring weaving.")))
   ofile))

(defun pm--run-command-in-ESS (command &optional from to)
  (require 'ess)
  (let ((ess-eval-visibly t)
        ;; R specific, should change eventually
        (ess-dialect "R")
        (weaver (symbol-value (oref pm/config :weaver))))
    (ess-force-buffer-current)
    (ess-process-put 'pm-output-file pm--output-file)
    (ess-process-put 'callbacks (list (oref weaver :callback)))
    (ess-process-put 'interruptable? t)
    (ess-process-put 'running-async? t)
    (ess-eval-linewise command)))

(provide 'poly-R)
