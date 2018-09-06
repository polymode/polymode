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


;; HOST MODES

(defcustom pm-host/ada
  (pm-host-chunkmode :name "ada"
                     :mode 'ada-mode)
  "Ada hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/coffee
  (pm-host-chunkmode :name "coffee"
                     :mode 'coffee-mode)
  "Coffee hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/fundamental
  (pm-host-chunkmode :name "fundamental"
                     :mode 'fundamental-mode)
  "Fundamental host mode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/java
  (pm-host-chunkmode :name "js"
                     :mode 'java-mode)
  "Java hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/js
  (pm-host-chunkmode :name "js"
                     :mode 'js-mode)
  "Javascript hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/latex
  (pm-host-chunkmode :name "latex"
                     :mode 'latex-mode)
  "Latex hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/html
  (pm-host-chunkmode :name "html"
                     :mode 'html-mode)
  "HTML hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/R
  (pm-host-chunkmode :name "R"
                     :mode 'r-mode)
  "R hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/perl
  (pm-host-chunkmode :name "perl"
                     :mode 'perl-mode)
  "Perl hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/ruby
  (pm-host-chunkmode :name "ruby"
                     :mode 'ruby-mode)
  "Ruby hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/pascal
  (pm-host-chunkmode :name "pascal"
                     :mode 'pascal-mode)
  "Pascal hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/C++
  (pm-host-chunkmode :name "C++"
                     :mode 'c++-mode
                     :protect-font-lock nil)
  "C++ hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/sgml
  (pm-host-chunkmode :name "sgml"
                     :mode 'sgml-mode)
  "SGML hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/text
  (pm-host-chunkmode :name "text"
                     :mode 'text-mode)
  "Text hostmode."
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-host/yaml
  (pm-host-chunkmode :name "YAML"
                     :mode 'yaml-mode)
  "YAML chunkmode."
  :group 'poly-hostmodes
  :type 'object)


;;; ROOT POLYMODES

;; These are simple generic configuration objects. More specialized polymodes
;; should clone these.

(defcustom pm-poly/brew
  (pm-polymode :name "brew"
               :hostmode 'pm-host/text)
  "Brew configuration."
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/html
  (pm-polymode :name "html"
               :hostmode 'pm-host/html)
  "HTML configuration."
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/C++
  (pm-polymode :name "C++"
               :hostmode 'pm-host/C++)
  "C++ configuration."
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/latex
  (pm-polymode :name "latex"
               :hostmode 'pm-host/latex)
  "LaTeX configuration."
  :group 'polymodes
  :type 'object)

(provide 'polymode-base)
;;; polymode-base.el ends here
