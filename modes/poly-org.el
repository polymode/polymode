;;; poly-org.el
;;
;; Filename: poly-org.el
;; Author: Spinu Vitalie
;; Maintainer: Spinu Vitalie
;; Copyright (C) 2013-2018 Spinu Vitalie
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
;;
;; Code:

(require 'org-src)
(require 'polymode)

(defcustom pm-host/org
  (pm-host-chunkmode "Org mode"
                     :mode 'org-mode)
  "Org host chunkmode."
  :group 'hostmodes
  :type 'object)

(defcustom  pm-inner/org
  (pm-inner-auto-chunkmode "org"
                           :head-matcher "^[ \t]*#\\+begin_src .*$"
                           :tail-matcher "^[ \t]*#\\+end_src"
                           :head-mode 'host
                           :tail-mode 'host
                           :head-matcher "#\\+begin_src +\\(\\(\\w\\|\\s_\\)+\\)"
                           :indent-offset org-edit-src-content-indentation)
  "Org typical chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/org
  (pm-polymode "org"
               :hostmode 'pm-host/org
               :innermodes '(pm-inner/org))
  "Org typical polymode configuration."
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-org-mode "poly-org")
(define-polymode poly-org-mode pm-poly/org)

(provide 'poly-org)
