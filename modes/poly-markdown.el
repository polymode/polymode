;;; poly-markdown.el
;;
;; Filename: poly-markdown.el
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
;; (require 'markdown-mode)

(defcustom pm-host/markdown
  (pm-bchunkmode "Markdown"
                 :mode 'markdown-mode
                 :init-functions '(poly-markdown-remove-markdown-hooks))
  "Markdown host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom  pm-inner/markdown
  (pm-hbtchunkmode-auto "markdown"
                        :head-reg "^[ \t]*```[{ \t]*\\w.*$"
                        :tail-reg "^[ \t]*```[ \t]*$"
                        :retriever-regexp "```[ \t]*\\(?:{\\|lang=\\)?\\([^ \t\n,}]+\\)"
                        :font-lock-narrow t)
  "Markdown typical chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/markdown
  (pm-polymode-multi-auto "markdown"
                          :hostmode 'pm-host/markdown
                          :auto-innermode 'pm-inner/markdown)
  "Markdown typical configuration"
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-markdown-mode "poly-markdown")
(define-polymode poly-markdown-mode pm-poly/markdown)

;;; FIXES:
(defun poly-markdown-remove-markdown-hooks ()
  ;; get rid of awful hooks
  (remove-hook 'window-configuration-change-hook 'markdown-fontify-buffer-wiki-links t)
  (remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link t))


(provide 'poly-markdown)
