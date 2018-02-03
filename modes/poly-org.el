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

(defun poly-org-mode-fix-outlines (oldbuf newbuf)
  "Set `outline-regexp' and `outline-level' in sub-modes of `poly-org-mode'.
This makes navigation easier, and is required in order for `poly-org-mode-process-buffer-name'
to be able to find the process buffer associated with the current code block."
  (let* ((host (oref pm/polymode -hostmode))
	 (hostbuf (oref host -buffer)))
    (if (eq (oref host :mode) 'org-mode)
	(setq outline-regexp (with-current-buffer
				 hostbuf outline-regexp)
	      outline-level (with-current-buffer
				hostbuf 'org-outline-level)))))

;; Run `poly-org-mode-fix-outlines' whenever we navigate into a code block in org-mode.
(add-hook 'polymode-switch-buffer-hook #'poly-org-mode-fix-outlines)
;;(remove-hook 'polymode-switch-buffer-hook #'poly-org-mode-fix-outlines)

(defun poly-org-mode-process-buffer-name (&optional a b c d e)
  "Return the name of the process buffer associated with the current babel block.
A, B, C, D, & E are placeholder arguments for compatibility with functions that are
advised by this function. This function will not work properly unless `outline-regexp',
 and `outline-level' are set correctly."
  (let ((cntx (org-element-context)))
    (if (memq (org-element-type cntx) '(inline-src-block src-block))
	(let* ((lang (org-element-property :language cntx))
	       (params (mapcar (lambda (s) (if (equal (substring s 0 1) ":")
					       (intern s)
					     s))
			       (split-string (org-element-property :parameters cntx) " " t)))
	       (session (plist-get params :session))
	       (dir (plist-get params :dir))
	       (kernel (plist-get params :kernel))
	       (default-directory
		 (or (and dir (file-name-as-directory dir)) default-directory))
	       (init-cmd (intern (format "org-babel-%s-initiate-session" lang)))
	       buf)
	  (when (and (stringp session) (string= session "none"))
	    (error "This block is not using a session!"))
	  (unless (fboundp init-cmd)
	    (error "No org-babel-initiate-session function for %s!" lang))
	  (setq buf (funcall init-cmd session (if kernel (list (cons :kernel kernel)))))
	  (if (bufferp buf) (buffer-name buf) buf)))))

;; Make sure that ess and python sub-modes are able to find the process buffer associated
;; with the code block.
(if (featurep 'ess)
    (advice-add 'ess-get-process :before-until #'poly-org-mode-process-buffer-name))
;;(advice-remove 'py--choose-buffer-name #'poly-org-mode-process-buffer-name)
(if (featurep 'python-mode)
    (advice-add 'py--choose-buffer-name :before-until #'poly-org-mode-process-buffer-name))
;;(advice-remove 'ess-get-process #'poly-org-mode-process-buffer-name)

(provide 'poly-org)
