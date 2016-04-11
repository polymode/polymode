;;; poly-noweb.el
;;
;; Filename: poly-noweb.el
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

(defcustom pm-poly/noweb
  (pm-polymode-one "noweb"
                   :hostmode 'pm-host/latex
                   :innermode 'pm-inner/noweb
                   :exporters '(pm-exporter/latexmk pm-exporter/pdflatex)
                   :map '(("<" . poly-noweb-electric-<)))
  "Noweb typical configuration"
  :group 'polymodes
  :type 'object)

(defcustom  pm-inner/noweb
  (pm-hbtchunkmode "noweb"
                   :head-reg "^[ \t]*<<\\(.*\\)>>="
                   :tail-reg "^[ \t]*@ *\\(%def.*\\)?$")
  "Noweb typical chunk."
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-noweb-mode "poly-noweb")
(define-polymode poly-noweb-mode pm-poly/noweb)

(defun poly-noweb-electric-< (arg)
  "Auto insert noweb chunk if at bol followed by white space.
If given an numerical argument, it simply insert `<'. Otherwise,
if at the beginning of a line in a host chunk insert \"<<>>=\", a
closing \"@\" and a newline if necessary."
  (interactive "P")
  (if (or arg (not (eq pm/type 'host)))
      (self-insert-command (if (numberp arg) arg 1))
    (if (not (looking-back "^[ \t]*"))
        (self-insert-command 1)
      (insert "<<")
      (save-excursion
        (insert ">>=\n\n@ ")
        (unless(looking-at "\\s *$")
          (newline)))
      (ess-noweb-update-chunk-vector))))

(defcustom pm-exporter/pdflatex
  (pm-shell-exporter "pdflatex"
                     :from
                     '(("latex" "\\.tex\\'" "LaTeX" "pdflatex -jobname %b %t %i"))
                     :to
                     '(("pdf"   "pdf"  "PDF" ""))
                     :quote t)
  "Shell pdflatex exporter."
  :group 'polymode-export
  :type 'object)

(defcustom pm-exporter/latexmk
  (pm-shell-exporter "latexmk"
                     :from
                     '(("latex" "\\.tex\\'" "LaTeX" "latexmk -jobname=%b %t %i"))
                     :to
                     '(("pdf"   "pdf"  "PDF" "-pdf")
                       ("ps"    "ps"  "PS" "-ps")
                       ("dvi"   "dvi"  "DVI" "-dvi"))
                     :quote t)
  "Shell latexmk dvi, ps and pdf exporter."
  :group 'polymode-export
  :type 'object)

(provide 'poly-noweb)
