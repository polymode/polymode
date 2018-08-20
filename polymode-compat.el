;;; polymode-compat.el --- Various compatibility fixes for other packages -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2018, Vitalie Spinu
;; Version: 0.1
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
;;; Commentary:
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode-core)
(require 'advice nil t)

(defgroup polymode-compat nil
  "Polymode compatibility settings."
  :group 'polymode)


;;; Various Wrappers for Around Advice

(defvar *span* nil)

;; advice doesn't provide named symbols. So we need to define specialized
;; wrappers for some key functions (unfinished)
(defmacro pm-define-wrapp-protected (fun)
  "Declare protected function with the name fun--pm-wrapped.
Return new name (symbol). FUN is an unquoted name of a function."
  (let* ((fun-name (symbol-name fun))
         (new-fun (intern (format "%s--pm-wrapped" fun-name)))
         (new-doc (format "  Error Protected function created with `pm-define-protected-wrapp'.\n\n%s"
                          (or (documentation fun) ""))))
    `(progn
       (defun ,new-fun (&rest args)
         ,new-doc
         (condition-case err
             (apply ',fun args)
           (error (message "(%s %s): %s"
                           ,fun-name
                           (mapconcat (lambda (x) (format "%s" x)) args " ")
                           (error-message-string err)))))
       ',new-fun)))

(defun pm-apply-protected (fun args)
  (when fun
    (condition-case-unless-debug err
        (apply fun args)
      (error (message "(%s %s): %s %s"
                      (if (symbolp fun)
                          (symbol-name fun)
                        "anonymous")
                      (mapconcat (lambda (x) (format "%s" x)) args " ")
                      (error-message-string err)
                      ;; (or (and (symbolp fun) "")
                      ;;     (replace-regexp-in-string "\n" "" (format "[%s]" fun)))
                      "[M-x pm-debug-mode RET for more info]"
                      )
             (when pm-debug-mode
               (backtrace))
             nil))))

(defun pm-override-output-position (orig-fun &rest args)
  "Restrict returned value of ORIG-FUN to fall into the current span.
*span* in `pm-map-over-spans` has precedence over span at point.
 ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((range (or (pm-span-to-range *span*)
                       (pm-get-innermost-range)))
            (pos (pm-apply-protected orig-fun args)))
        (and pos
             (min (max pos (car range))
                  (cdr range))))
    (apply orig-fun args)))


(defun pm-override-output-cons (orig-fun &rest args)
  "Restrict returned (beg . end) of ORIG-FUN to fall into the current span.
*span* in `pm-map-over-spans` has precedence over span at point.
This will break badly if (point) is not inside expected range.
ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((range (or (pm-span-to-range *span*)
                       (pm-get-innermost-range)))
            (be (pm-apply-protected orig-fun args)))
        (let ((out (and be
                        (cons (and (car be)
                                   (min (max (car be) (car range))
                                        (cdr range)))
                              (and (cdr be)
                                   (max (min (cdr be) (cdr range))
                                        (car range)))))))
          (when pm-verbose
            (message "(pm-override-output-cons %s) -> %s" args out))
          out))
    (apply orig-fun args)))

(defun pm-narrowed-override-output-cons (orig-fun &rest args)
  "Restrict returned (beg . end) of ORIG-FUN to fall into the current span.
Run ORIG-FUN with buffer narrowed to span. *span* in
`pm-map-over-spans` has precedence over span at point. ARGS are
passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let ((*span* (or *span* (pm-get-innermost-span))))
        (pm-with-narrowed-to-span *span*
          (apply #'pm-override-output-cons orig-fun args)))
    (apply orig-fun args)))

(defun pm-substitute-beg-end (orig-fun beg end &rest args)
  "Execute ORIG-FUN with first BEG and END arguments limited to current span.
*span* in `pm-map-over-spans` has precedence over span at point.
 ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (let* ((pos (if (and (<= (point) end) (>=  (point) beg))
                      (point)
                    end))
             (range (or (pm-span-to-range *span*)
                        (pm-get-innermost-range pos)))
             (new-beg (max beg (car range)))
             (new-end (min end (cdr range))))
        (pm-apply-protected orig-fun (append (list new-beg new-end) args)))
    (apply orig-fun beg end args)))

(defun pm-execute-narrowed-to-span (orig-fun &rest args)
  "Execute ORIG-FUN narrowed to the current span.
*span* in `pm-map-over-spans` has precedence over span at point.
 ARGS are passed to ORIG-FUN."
  (if (and polymode-mode pm/polymode)
      (pm-with-narrowed-to-span *span*
        (pm-apply-protected orig-fun args))
    (apply orig-fun args)))

(defun pm-execute-with-no-polymode-hooks (orig-fun &rest args)
  "Execute ORIG-FUN without allowing polymode core hooks.
That is, bind `pm-allow-post-command-hook' and
`pm-allow-after-change-hook' to nil. *span* in
`pm-map-over-spans' has precedence over span at point. ARGS are
passed to ORIG-FUN."
  ;; this advice is nowhere used yet
  (if (and polymode-mode pm/polymode)
      (let ((pm-allow-post-command-hook t)
            (pm-allow-after-change-hook t))
        ;; This advice might be useful when functions can switch buffers to work
        ;; inside the base buffer (like basic-save-buffer does). Thus, we sync
        ;; points first.
        (pm--synchronize-points)
        ;; save-excursion might be also often necessary
        (apply orig-fun args))
    (apply orig-fun args)))

(defun pm-execute-with-save-excursion (orig-fun &rest args)
  "Execute ORIG-FUN within ‘save-excursion’.
ARGS are passed to ORIG-FUN."
  ;; This advice is required when other functions switch buffers to work inside
  ;; base buffer and don't restore the point. For some not very clear reason
  ;; this seem to be necessary for save-buffer which saves buffer but not point.
  (if (and polymode-mode pm/polymode)
      (progn
        (pm--synchronize-points)
        (save-excursion
          (apply orig-fun args)))
    (apply orig-fun args)))


;;; Flyspel
(defun pm--flyspel-dont-highlight-in-chunkmodes (beg end poss)
  (or (car (get-text-property beg :pm-span))
      (car (get-text-property end :pm-span))))


;;; C/C++/Java
(pm-around-advice 'c-before-context-fl-expand-region #'pm-override-output-cons)
(pm-around-advice 'c-state-semi-safe-place #'pm-override-output-position)
;; (advice-remove 'c-state-semi-safe-place #'pm-override-output-position)
;; c-font-lock-fontify-region calls it directly
;; (pm-around-advice 'font-lock-default-fontify-region #'pm-substitute-beg-end)
(pm-around-advice 'c-determine-limit #'pm-execute-narrowed-to-span)


;;; Python
(defun pm--python-dont-indent-to-0 (fun)
  "Fix indent FUN not to cycle to 0 indentation."
  (if (and polymode-mode pm/type)
      (let ((last-command (unless (eq (pm--first-line-indent) (current-indentation))
                            last-command)))
        (funcall fun))
    (funcall fun)))

(pm-around-advice 'python-indent-line-function #'pm--python-dont-indent-to-0)


;;; Core Font Lock
(defun pm-check-for-real-change-in-extend-multiline (fun)
  "Protect FUN from inf-looping at ‘point-max’.
FUN is `font-lock-extend-region-multiline'. Propagate only real
changes."
  ;; fixme: report this ASAP!
  (let ((obeg font-lock-beg)
        (oend font-lock-end)
        (change (funcall fun)))
    (and change
         (not (eq obeg font-lock-beg))
         (not (eq oend font-lock-end)))))

(pm-around-advice 'font-lock-extend-region-multiline #'pm-check-for-real-change-in-extend-multiline)


;;; Editing
(pm-around-advice 'fill-paragraph #'pm-execute-narrowed-to-span)

;; `save-buffer` misbehaves because after each replacement modification hooks
;; are triggered and poly buffer is switched in unpredictable fashion.
;;
;; https://github.com/vspinu/polymode/issues/93 It can be
;; reproduced with (add-hook 'before-save-hook 'delete-trailing-whitespace nil
;; t) in the base buffer.
;;
;; save-excursion is probably not quite right fix for this but it seem to work
(pm-around-advice 'basic-save-buffer #'pm-execute-with-save-excursion)

;; Query replace were probably misbehaving due to unsaved match data.
;; (https://github.com/vspinu/polymode/issues/92) The following is probably not
;; necessary.
;; (pm-around-advice 'perform-replace 'pm-execute-inhibit-modification-hooks)


;;; EVIL

(defun polymode-switch-buffer-keep-evil-state-maybe (old-buffer new-buffer)
  (when (and (boundp 'evil-state)
             evil-state)
    (let ((old-state (buffer-local-value 'evil-state old-buffer))
          (new-state (buffer-local-value 'evil-state new-buffer)))
      (unless (eq old-state new-state)
        (with-current-buffer new-buffer
          (evil-change-state old-state))))))

(eval-after-load 'evil-core
  '(add-hook 'polymode-switch-buffer-hook 'polymode-switch-buffer-keep-evil-state-maybe))


(provide 'polymode-compat)

(provide 'polymode-compat)

;;; polymode-compat.el ends here
