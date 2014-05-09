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

(defcustom pm-base/org
  (pm-basemode "Org mode"
               :mode 'org-mode)
  "Org base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom  pm-chunk/org
  (pm-chunkmode-auto "org"
                     :head-reg "^[ \t]*#\\+begin_src .*$"
                     :tail-reg "^[ \t]*#\\+end_src"
                     :head-mode 'base
                     :tail-mode 'base
                     :retriever-regexp "#\\+begin_src +\\(\\(\\w\\|\\s_\\)+\\)"
                     :indent-offset org-edit-src-content-indentation
                     :font-lock-narrow t)
  "Org typical chunk."
  :group 'polymode-chunkmodes
  :type 'object)

(defcustom pm-config/org
  (pm-config-multi-auto "org"
                        :basemode 'pm-base/org
                        :auto-chunkmode 'pm-chunk/org)
  "Org typical configuration"
  :group 'polymode-configs
  :type 'object)

;;;###autoload  (autoload 'poly-org-mode "poly-org")
(define-polymode poly-org-mode pm-config/org)

(provide 'poly-org)

