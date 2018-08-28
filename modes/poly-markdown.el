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
  (pm-host-chunkmode :object-name "Markdown"
                     :mode 'markdown-mode
                     :init-functions '(poly-markdown-remove-markdown-hooks)
                     :protect-syntax nil
                     :protect-font-lock nil)
  "Markdown host chunkmode"
  :group 'poly-host-modes
  :type 'object)

(defcustom pm-inner/markdown-fenced-code
  (pm-inner-auto-chunkmode :object-name "markdown-fenced-code"
                           :head-matcher "^[ \t]*```[{ \t]*\\w.*$"
                           :tail-matcher "^[ \t]*```[ \t]*$"
                           :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1)
                           :head-mode 'host
                           :tail-mode 'host)
  "Markdown fenced code block."
  :group 'poly-inner-modes
  :type 'object)

(defcustom pm-inner/markdown-inline-code
  (pm-inner-auto-chunkmode :object-name "markdown-inline-code"
                           :head-matcher (cons "[^`]\\(`{?[[:alpha:]+-]+\\)[ \t]" 1)
                           :tail-matcher (cons "[^`]\\(`\\)[^`]" 1)
                           :mode-matcher (cons "`[ \t]*{?\\(?:lang *= *\\)?\\([[:alpha:]+-]+\\)" 1)
                           :head-mode 'host
                           :tail-mode 'host)
  "Markdown inline code."
  :group 'poly-inner-modes
  :type 'object)

(defcustom pm-inner/markdown-displayed-math
  (pm-inner-chunkmode :object-name "markdown-displayed-math"
                      :head-matcher (cons "^[ \t]*\\(\\$\\$\\)." 1)
                      :tail-matcher (cons "[^$]\\(\\$\\$\\)[^$[:alnum:]]" 1)
                      :head-mode 'host
                      :tail-mode 'host
                      :mode 'latex-mode)
  "Displayed math $$..$$ block.
Tail must be flowed by new line but head not (a space or comment
character would do)."
  :group 'poly-inner-modes
  :type 'object)

(defcustom pm-inner/markdown-inline-math
  (pm-inner-chunkmode :object-name "markdown-inline-math"
                      :head-matcher (cons "[ \t\n]\\(\\$\\)[^ $\t[:digit:]]" 1)
                      :tail-matcher (cons "[^ $\\\t]\\(\\$\\)[^$[:alnum:]]" 1)
                      :head-mode 'host
                      :tail-mode 'host
                      :mode 'latex-mode)
  "Displayed math $$..$$ block.
Tail must be flowed by new line but head not (a space or comment
character would do)."
  :group 'poly-inner-modes
  :type 'object)

(defcustom pm-poly/markdown
  (pm-polymode :object-name "markdown"
               :hostmode 'pm-host/markdown
               :innermodes '(pm-inner/markdown-fenced-code
                             pm-inner/markdown-inline-code
                             pm-inner/markdown-displayed-math
                             pm-inner/markdown-inline-math))
  "Markdown typical configuration"
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-markdown-mode "poly-markdown")
(define-polymode poly-markdown-mode pm-poly/markdown)
(add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))

;;; FIXES:
(defun poly-markdown-remove-markdown-hooks ()
  ;; get rid of awful hooks
  (remove-hook 'window-configuration-change-hook 'markdown-fontify-buffer-wiki-links t)
  (remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link t))

(with-eval-after-load "markdown-mode"
;;; https://github.com/jrblevin/markdown-mode/pull/356
  (defun markdown-match-propertized-text (property last)
    "Match text with PROPERTY from point to LAST.
Restore match data previously stored in PROPERTY."
    (let ((saved (get-text-property (point) property))
          pos)
      (unless saved
        (setq pos (next-single-property-change (point) property nil last))
        (unless (= pos last)
          (setq saved (get-text-property pos property))))
      (when saved
        (set-match-data saved)
        ;; Step at least one character beyond point. Otherwise
        ;; `font-lock-fontify-keywords-region' infloops.
        (goto-char (min (1+ (max (match-end 0) (point)))
                        (point-max)))
        saved))))

(provide 'poly-markdown)
