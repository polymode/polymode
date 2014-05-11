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

(defcustom pm-config/noweb
  (pm-config-one "noweb"
                 :basemode 'pm-base/latex
                 :chunkmode 'pm-chunk/noweb
                 :exporters '(pm-exporter/latexmk pm-exporter/pdflatex)
                 :map '(("<" . poly-noweb-electric-<)))
  "Noweb typical configuration"
  :group 'polymode
  :type 'object)

(defcustom  pm-chunk/noweb
  (pm-chunkmode "noweb"
                :head-reg  "<<\\(.*\\)>>="
                :tail-reg    "\\(@ +%def .*\\)$\\|\\(@[ \n]\\)")
  "Noweb typical chunk."
  :group 'polymode
  :type 'object)

;;;###autoload (autoload 'poly-noweb-mode "poly-noweb")
(define-polymode poly-noweb-mode pm-config/noweb)

(defun poly-noweb-electric-< (arg)
  "Auto insert noweb chunk if at bol followed by white space.
If given an numerical argument, it simply insert `<'. Otherwise,
if at the beginning of a line in a base chunk insert \"<<>>=\", a
closing \"@\" and a newline if necessary."
  (interactive "P")
  (if (or arg (not (eq pm/type 'base)))
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
                     '(("latex" "\\.tex\\'" "LaTeX" "latexmk -jobname %O %t %i"))
                     :to
                     '(("pdf"  	"pdf"  "PDF" "")))
  "Shell pdflatex exporter."
  :group 'polymode-export
  :type 'object)

(defcustom pm-exporter/latexmk
  (pm-shell-exporter "latexmk"
                   :from
                   '(("latex" "\\.tex\\'" "LaTeX" "latexmk -jobname=%O %t %i"))
                   :to
                   '(("dvi"  	"dvi"  "DVI" "-dvi")
                     ("pdf"  	"pdf"  "PDF" "-pdf")
                     ("ps"  	"ps"  "PS" "-ps")))
  "Shell latexmk dvi, ps and pdf exporter."
  :group 'polymode-export
  :type 'object)

(provide 'poly-noweb)
