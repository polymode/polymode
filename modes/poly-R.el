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

(defcustom pm-poly/R
  (pm-polymode-one "R"
                   :hostmode 'pm-host/R
                   :innermode 'pm-inner/fundamental)
  "R root polymode. Not intended to be used directly."
  :group 'polymodes
  :type 'object)

;; NOWEB
(require 'poly-noweb)
(defcustom pm-poly/noweb+R
  (clone pm-poly/noweb :innermode 'pm-inner/noweb+R)
  "Noweb for R configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-inner/noweb+R
  (clone pm-inner/noweb
         :mode 'R-mode)
  "Noweb for R"
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-noweb+r-mode "poly-R")
(define-polymode poly-noweb+r-mode pm-poly/noweb+R :lighter " PM-Rnw")



;; MARKDOWN
(require 'poly-markdown)
;;;###autoload (autoload 'poly-markdown+r-mode "poly-R")
(define-polymode poly-markdown+r-mode pm-poly/markdown :lighter " PM-Rmd")


;; RAPPORT
(defcustom pm-poly/rapport
  (clone pm-poly/markdown "rapport"
         :innermodes '(pm-inner/brew+R
                       pm-inner/rapport+YAML))
  "Rapport template configuration"
  :group 'polymodes
  :type 'object)

(defcustom  pm-inner/rapport+YAML
  (pm-hbtchunkmode "rapport+YAML"
                   :mode 'yaml-mode
                   :head-reg "<!--head"
                   :tail-reg "head-->")
  "YAML header in Rapport files"
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-rapport-mode "poly-R")
(define-polymode poly-rapport-mode pm-poly/rapport nil)



;; HTML
(defcustom pm-poly/html+R
  (clone pm-poly/html "html+R" :innermode 'pm-inner/html+R)
  "HTML + R configuration"
  :group 'polymodes
  :type 'object)

(defcustom  pm-inner/html+R
  (pm-hbtchunkmode "html+R"
                   :mode 'R-mode
                   :head-reg "<!--[ \t]*begin.rcode"
                   :tail-reg "end.rcode[ \t]*-->")
  "HTML KnitR innermode."
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-html+r-mode "poly-R")
(define-polymode poly-html+r-mode pm-poly/html+R)



;;; R-brew
(defcustom pm-poly/brew+R
  (clone pm-poly/brew "brew+R"
         :innermode 'pm-inner/brew+R)
  "Brew + R configuration"
  :group 'polymodes
  :type 'object)

(defcustom  pm-inner/brew+R
  (pm-hbtchunkmode "brew+R"
                   :mode 'R-mode
                   :head-reg "<%[=%]?"
                   :tail-reg "[#=%=-]?%>")
  "Brew R chunk."
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-brew+r-mode "poly-R")
(define-polymode poly-brew+r-mode pm-poly/brew+R)



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

(defcustom pm-poly/R+C++
  (clone pm-poly/R "R+C++" :innermode 'pm-inner/R+C++)
  "R + C++ configuration"
  :group 'polymodes
  :type 'object)

(defcustom  pm-inner/R+C++
  (pm-hbtchunkmode "R+C++"
                   :mode 'c++-mode
                   :head-mode 'host
                   :head-reg 'pm--R+C++-head-matcher
                   :tail-reg 'pm--R+C++-tail-matcher
                   :font-lock-narrow nil)
  "HTML KnitR chunk."
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-r+c++-mode "poly-R")
(define-polymode poly-r+c++-mode pm-poly/R+C++)



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

(defcustom pm-poly/C++R
  (clone pm-poly/C++ "C++R" :innermode 'pm-inner/C++R)
  "R + C++ configuration"
  :group 'polymodes
  :type 'object)

(defcustom  pm-inner/C++R
  (pm-hbtchunkmode "C++R"
                   :mode 'R-mode
                   :head-reg 'pm--C++R-head-matcher
                   :tail-reg 'pm--C++R-tail-matcher)
  "HTML KnitR chunk."
  :group 'polymodes
  :type 'object)

;;;###autoload (autoload 'poly-c++r-mode "poly-R")
(define-polymode poly-c++r-mode pm-poly/C++R)



;;; R help
(defcustom pm-poly/ess-help+R
  (pm-polymode-one "ess-R-help"
                   :innermode 'pm-inner/ess-help+R)
  "ess-R-help"
  :group 'polymodes
  :type 'object)

(defcustom  pm-inner/ess-help+R
  (pm-hbtchunkmode "ess-help+R"
                   :mode 'R-mode
                   :head-reg "^Examples:"
                   :tail-reg "\\'"
                   :indent-offset 5
                   :switch-buffer-functions '(pm--ess-help+R-turn-off-read-only))
  "Ess help R chunk"
  :group 'innermodes
  :type 'object)

(defun pm--ess-help+R-turn-off-read-only (&rest ignore)
  ;; don't transfer read only status from main help buffer
  (cl-case pm/type
    (body (read-only-mode -1))
    (head (read-only-mode 1))))

;;;###autoload (autoload 'poly-ess-help+r-mode "poly-R")
(define-polymode poly-ess-help+r-mode pm-poly/ess-help+R)

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

(defcustom pm-poly/Rd
  (pm-polymode-one "R-documentation"
                   :innermode 'pm-inner/Rd)
  "R polymode for Rd files"
  :group 'polymodes
  :type 'object)

(defcustom pm-inner/Rd
  (pm-hbtchunkmode "R+C++"
                   :mode 'R-mode
                   :head-mode 'host
                   :head-reg 'pm--Rd-examples-head-matcher
                   :tail-reg 'pm--Rd-examples-tail-matcher)
  "Rd examples chunk."
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-Rd-mode "poly-R")
(define-polymode poly-Rd-mode pm-poly/Rd)
(add-hook 'Rd-mode-hook 'poly-Rd-mode)



;; Rmarkdown
(defcustom pm-exporter/Rmarkdown
  (pm-shell-exporter "Rmarkdown"
                     :from
                     '(("Rmarkdown"  "\\.[rR]?md\\|rapport\\'" "R Markdown"
                        "Rscript -e \"rmarkdown::render('%i', output_format = '%t', output_file = '%o')\""))
                     :to
                     '(("auto" . pm--rmarkdown-shell-auto-selector)
                       ("html" "html" "html document" "html_document")
                       ("pdf" "pdf" "pdf document" "pdf_document")
                       ("word" "docx" "word document" "word_document")
                       ("md" "md" "md document" "md_document")
                       ("ioslides" "html" "ioslides presentation" "ioslides_presentation")
                       ("slidy" "html" "slidy presentation" "slidy_presentation")
                       ("beamer" "pdf" "beamer presentation" "beamer_presentation")))
  "R Markdown exporter.
Please not that with 'AUTO DETECT' export options, output file
names are inferred by Rmarkdown from YAML description
block. Thus, output file names don't comply with
`polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(polymode-register-exporter pm-exporter/Rmarkdown nil
                            pm-poly/markdown pm-poly/rapport)

(defun pm--rmarkdown-shell-auto-selector (action &rest ignore)
  (cl-case action
    (doc "AUTO DETECT")
    (command "Rscript -e \"rmarkdown::render('%i', output_format = 'all')\"")
    (output-file #'pm--rmarkdown-output-file-sniffer)))

(defcustom pm-exporter/Rmarkdown-ESS
  (pm-callback-exporter "Rmarkdown-ESS"
                        :from
                        '(("Rmarkdown" "\\.[rR]?md\\|rapport\\'" "R Markdown"
                           "rmarkdown::render('%I', output_format = '%t', output_file = '%O')\n"))
                        :to
                        '(("auto" . pm--rmarkdown-callback-auto-selector)
                          ("html" "html" "html document" "html_document")
                          ("pdf" "pdf" "pdf document" "pdf_document")
                          ("word" "docx" "word document" "word_document")
                          ("md" "md" "md document" "md_document")
                          ("ioslides" "html" "ioslides presentation" "ioslides_presentation")
                          ("slidy" "html" "slidy presentation" "slidy_presentation")
                          ("beamer" "pdf" "beamer presentation" "beamer_presentation"))
                        :function 'pm--ess-run-command
                        :callback 'pm--ess-callback)
  "R Markdown exporter.
Please not that with 'AUTO DETECT' export options, output file
names are inferred by Rmarkdown from YAML description
block. Thus, output file names don't comply with
`polymode-exporter-output-file-format'."
  :group 'polymode-export
  :type 'object)

(polymode-register-exporter pm-exporter/Rmarkdown-ESS nil
                            pm-poly/markdown pm-poly/rapport)

(defun pm--rmarkdown-callback-auto-selector (action &rest ignore)
  (cl-case action
    (doc "AUTO DETECT")
    ;; last file is not auto-detected unless we cat new line
    (command "rmarkdown::render('%I', output_format = 'all')")
    (output-file #'pm--rmarkdown-output-file-sniffer)))

(defun pm--rmarkdown-output-file-sniffer ()
  (goto-char (point-min))
  (let (files)
    (while (re-search-forward "Output created: +\\(.*\\)" nil t)
      (push (expand-file-name (match-string 1)) files))
    (reverse (delete-dups files))))


;; KnitR
(defcustom pm-weaver/knitR
  (pm-shell-weaver "knitr"
                   :from-to
                   '(("latex" "\\.\\(tex\\|[rR]nw\\)\\'" "tex" "LaTeX" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("html" "\\.x?html?\\'" "html" "HTML" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("markdown" "\\.[rR]?md]\\'" "md" "Markdown" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("rst" "\\.rst" "rst" "ReStructuredText" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("brew" "\\.[rR]?brew\\'" "brew" "Brew" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("asciidoc" "\\.asciidoc\\'" "txt" "AsciiDoc" "Rscript -e \"knitr::knit('%i', output='%o')\"")
                     ("textile" "\\.textile\\'" "textile" "Textile" "Rscript -e \"knitr::knit('%i', output='%o')\"")))
  "Shell knitR weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/knitR nil
                          pm-poly/noweb+R pm-poly/markdown
                          pm-poly/rapport pm-poly/html+R)

(defcustom pm-weaver/knitR-ESS
  (pm-callback-weaver "knitR-ESS"
                      :from-to
                      '(("latex" "\\.\\(tex\\|rnw\\)\\'" "tex" "LaTeX" "knitr::knit('%I', output='%O')")
                        ("html" "\\.x?html?\\'" "html" "HTML" "knitr::knit('%I', output='%O')")
                        ("markdown" "\\.r?md\\'" "md" "Markdown" "knitr::knit('%I', output='%O')")
                        ("rst" "\\.rst\\'" "rst" "ReStructuredText" "knitr::knit('%I', output='%O')")
                        ("brew" "\\.r?brew\\'" "brew" "Brew" "knitr::knit('%I', output='%O')")
                        ("asciidoc" "\\.asciidoc\\'" "txt" "AsciiDoc" "knitr::knit('%I', output='%O')")
                        ("textile" "\\.textile\\'" "textile" "Textile" "knitr::knit('%I', output='%O')"))
                      :function 'pm--ess-run-command
                      :callback 'pm--ess-callback)
  "ESS knitR weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/knitR-ESS nil
                          pm-poly/noweb+R pm-poly/markdown
                          pm-poly/rapport pm-poly/html+R)

(defcustom pm-weaver/Sweave-ESS
  (pm-callback-weaver "ESS-Sweave"
                      :from-to '(("latex" "\\.\\(tex\\|r?s?nw\\)\\'" "tex"
                                  "LaTeX" "Sweave('%I', output='%O')"))
                      :function 'pm--ess-run-command
                      :callback 'pm--ess-callback)
  "ESS 'Sweave' weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/Sweave-ESS nil
                          pm-poly/noweb+R)


;; Sweave
(defcustom pm-weaver/Sweave
  (pm-shell-weaver "sweave"
                   :from-to
                   '(("latex" "\\.\\(tex\\|r?s?nw\\)\\'"
                      "tex" "LaTeX" "R CMD Sweave %i --options=\"output='%o'\"")))
  "Shell 'Sweave' weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/Sweave nil
                          pm-poly/noweb+R)


;; ESS command

(declare-function ess-async-command nil)
(declare-function ess-force-buffer-current nil)
(declare-function ess-process-get nil)
(declare-function ess-process-put nil)
(declare-function comint-previous-prompt nil)

(defun pm--ess-callback (proc string)
  (let ((ofile (process-get proc :output-file)))
    ;; This is getting silly. Ess splits output for optimization reasons. So we
    ;; are collecting output from 3 places:
    ;;   - most recent STRING
    ;;   - string in accumulation buffer 'accum-buffer-name
    ;;   - string already in output buffer
    (with-current-buffer (process-get proc 'accum-buffer-name)
      (setq string (concat (buffer-substring (point-min) (point-max))
                           string)))
    (with-current-buffer (process-buffer proc)
      (setq string (concat (buffer-substring (or ess--tb-last-input (comint-previous-prompt)) (point-max))
                           string)))
    (with-temp-buffer
      (insert string)
      (when (string-match-p "Error\\(:\\| +in\\)" string)
        (error "Errors durring ESS async command"))
      (unless (stringp ofile)
        (setq ofile (funcall ofile))))
    ofile))

(defun pm--ess-run-command (command callback &rest ignore)
  (require 'ess)
  (let ((ess-eval-visibly t)
        (ess-dialect "R"))
    (ess-force-buffer-current)
    (ess-process-put :output-file pm--output-file)
    (ess-process-put 'callbacks (list callback))
    (ess-process-put 'interruptable? t)
    (ess-process-put 'running-async? t)
    (ess-eval-linewise command)))


;; COMPAT

(when (fboundp 'advice-add)
  (advice-add 'ess-eval-paragraph :around 'pm-execute-narrowed-to-span)
  (advice-add 'ess-eval-buffer :around 'pm-execute-narrowed-to-span)
  (advice-add 'ess-beginning-of-function :around 'pm-execute-narrowed-to-span))

(provide 'poly-R)
