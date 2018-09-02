;;; polymode-base.el --- Root Host and Polymode Configuration Objects -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013-2018, Vitalie Spinu
;; Author: Vitalie Spinu
;; URL: https://github.com/vspinu/polymode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;; Code:

(require 'polymode-classes)

;;;###autoload
(defvar-local polymode-default-inner-mode nil
  "Inner mode for chunks with unspecified modes.
Intended to be used as local variable in polymode buffers.")
;;;###autoload
(put 'polymode-default-inner-mode 'safe-local-variable 'symbolp)


;; HOST MODES

(defcustom pm-host/coffee
  (pm-host-chunkmode :name "coffee"
                     :mode 'coffee-mode)
  "Coffee host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/fundamental
  (pm-host-chunkmode :name "fundamental"
                     :mode 'fundamental-mode)
  "Fundamental host mode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/js
  (pm-host-chunkmode :name "js"
                     :mode 'js-mode)
  "Javascript host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/java
  (pm-host-chunkmode :name "js"
                     :mode 'java-mode)
  "Java host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/latex
  (pm-host-chunkmode :name "latex"
                     :mode 'latex-mode)
  "Latex host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/html
  (pm-host-chunkmode :name "html"
                     :mode 'html-mode)
  "HTML host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/R
  (pm-host-chunkmode :name "R"
                     :mode 'R-mode)
  "R host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/C++
  (pm-host-chunkmode :name "C++"
                     :mode 'c++-mode
                     :protect-font-lock nil)
  "C++ host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/text
  (pm-host-chunkmode :name "text"
                     :mode 'text-mode)
  "Text host chunkmode."
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-host/yaml
  (pm-host-chunkmode :name "YAML"
                     :mode 'yaml-mode)
  "YAML chunkmode."
  :group 'poly-host-modes
  :type 'object)



;;; ROOT POLYMODES

;; These are simple generic configuration objects. More specialized polymodes
;; should clone these.

(defcustom pm-poly/brew
  (pm-polymode :name "brew"
               :hostmode 'pm-host/text
               :innermodes nil)
  "Typical Brew configuration."
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/html
  (pm-polymode :name "html"
               :hostmode 'pm-host/html
               :innermodes nil)
  "HTML typical configuration."
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/C++
  (pm-polymode :name "C++"
               :hostmode 'pm-host/C++)
  "C++ typical configuration."
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/latex
  (pm-polymode :name "latex"
               :hostmode 'pm-host/latex)
  "LaTeX typical configuration."
  :group 'polymodes
  :type 'object)

(provide 'polymode-base)
;;; polymode-base.el ends here
