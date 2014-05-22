;;; poly-org.el
;;
;; Filename: poly-org.el
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

(require 'org-src)
(require 'polymode)

(defcustom pm-host/org
  (pm-bchunkmode "Org mode"
               :mode 'org-mode)
  "Org host innermode"
  :group 'hostmodes
  :type 'object)

(defcustom  pm-inner/org
  (pm-hbtchunkmode-auto "org"
                     :head-reg "^[ \t]*#\\+begin_src .*$"
                     :tail-reg "^[ \t]*#\\+end_src"
                     :head-mode 'host
                     :tail-mode 'host
                     :retriever-regexp "#\\+begin_src +\\(\\(\\w\\|\\s_\\)+\\)"
                     :indent-offset org-edit-src-content-indentation
                     :font-lock-narrow t)
  "Org typical chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/org
  (pm-polymode-multi-auto "org"
                        :hostmode 'pm-host/org
                        :auto-innermode 'pm-inner/org)
  "Org typical configuration"
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-org-mode "poly-org")
(define-polymode poly-org-mode pm-poly/org)

(provide 'poly-org)

