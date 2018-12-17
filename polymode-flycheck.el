;;; polymode-flycheck.el --- Flycheck helpers for polymodes -*- lexical-binding: t -*-
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

(require 'polymode-core)
(require 'polymode-classes)
(require 'flycheck)

(defgroup polymode-flycheck nil
  "Polymode Tanglers."
  :group 'polymode)


(defun polymode-flycheck--collect-bodies-of-mode (mode buffer-name)
  "Insert into a buffer with name `buffer-name' all spans with a
major-mode equal to `mode'. Create special text properties to
remember original positions of spans in a host file. Return
buffer object."
  (save-excursion
    (let* ((temp-buffer (get-buffer-create buffer-name))
           (base (pm-base-buffer))
           temp-beg temp-end
           sbeg send sline)
      (pm-map-over-spans
       (lambda (span)
         (when (and (eq (car span) 'body)
                  (eq mode major-mode))
           (setq sbeg (nth 1 span)
                 send (nth 2 span)
                 sline (line-number-at-pos sbeg))
           (with-current-buffer temp-buffer
             (setq temp-beg (point)
                   temp-end (+ temp-beg (- send sbeg)))
             (insert-buffer-substring-no-properties base sbeg send)
             ;;save the first line number of the body in host file (sline)
             ;;and in temp file
             (set-text-properties temp-beg temp-end
                                  `(pm-orig-line ,sline
                                    pm-temp-line ,(line-number-at-pos temp-beg)))))))
      temp-buffer)))

(defun polymode-flycheck--temp-buffer-name (&optional buffer)
  (concat (buffer-name buffer) "-polymode-flycheck"))

(defun polymode-flycheck--make-temp-buffer ()
  "Create a temp buffer if it does not exist yet.
See `polymode-flycheck--collect-bodies-of-mode'."
  (let* ((name (polymode-flycheck--temp-buffer-name))
         (buffer (get-buffer name)))
    (if buffer
        buffer
      (polymode-flycheck--collect-bodies-of-mode major-mode name))))

(defun polymode-flycheck--get-buffer-transformer ()
  "Return transformer if polymode is on and point is in a
indirect buffer."
  (if (and (bound-and-true-p polymode-mode)
         (buffer-base-buffer))
      #'polymode-flycheck--make-temp-buffer
    nil))

;;advice two flycheck functions to transform the polymode buffer before checking content.
(defadvice flycheck-save-buffer-to-file (around advice-maybe-run-buffer-transform first (file-name) activate)
  (if-let* ((tfm (polymode-flycheck--get-buffer-transformer))
            (transformed-buf (funcall tfm)))
      (with-current-buffer transformed-buf
        ad-do-it)
    ad-do-it))

(defadvice flycheck-process-send-buffer (around advice-maybe-run-buffer-transform first (process) activate)
  (if-let* ((tfm (polymode-flycheck--get-buffer-transformer))
            (transformed-buf (funcall tfm)))
      (with-current-buffer transformed-buf
        ad-do-it)
    ad-do-it))

(defun polymode-flycheck--adjust-line-numbers (err)
  "Restore line numbers, so they match host file."
  (when-let* ((temp-buffer-name
               (polymode-flycheck--temp-buffer-name (flycheck-error-buffer err)))
              (temp-buffer (get-buffer temp-buffer-name)))
    (with-current-buffer temp-buffer
      (goto-char (point-min))
      (forward-line (- (flycheck-error-line err) 1))
      (when-let* ((err-pos (line-beginning-position))
                  (orig-line (get-text-property err-pos 'pm-orig-line))
                  (temp-span-beg-line (get-text-property err-pos 'pm-temp-line)))

        (setf (flycheck-error-line err) (+ orig-line (- (flycheck-error-line err) temp-span-beg-line)))
        ;;return nil, so flycheck can proceed errors processing
        nil))))

(defun polymode-flycheck--kill-temp-buffer ()
  (when-let ((buffer (get-buffer (polymode-flycheck--temp-buffer-name))))
    (kill-buffer buffer)))

(add-hook 'flycheck-after-syntax-check-hook #'polymode-flycheck--kill-temp-buffer)
(add-hook 'flycheck-process-error-functions #'polymode-flycheck--adjust-line-numbers)



(provide 'polymode-flycheck)
;;; polymode-flycheck.el ends here
