;;; polymode.el --- Extensible framework for multiple major modes -*- lexical-binding: t -*-
;;
;; Author: Vitalie Spinu
;; Maintainer: Vitalie Spinu
;; Copyright (C) 2013-2018, Vitalie Spinu
;; Version: 0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/vitoshka/polymode
;; Keywords: languages, multi-modes, processes
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
;;   Documentation at https://polymode.github.io
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode-core)
(require 'polymode-classes)
(require 'polymode-methods)
(require 'polymode-compat)
(require 'polymode-export)
(require 'polymode-weave)
(require 'polymode-base)
(require 'poly-lock)
(eval-when-compile
  (require 'derived))

(defcustom polymode-prefix-key "\M-n"
  "Prefix key for the polymode mode keymap.
Not effective after loading the polymode library."
  :group 'polymode
  :type '(choice string vector))

(defvar polymode-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map polymode-prefix-key
      (let ((map (make-sparse-keymap)))
        ;; navigation
        (define-key map "\C-n" 'polymode-next-chunk)
        (define-key map "\C-p" 'polymode-previous-chunk)
        (define-key map "\C-\M-n" 'polymode-next-chunk-same-type)
        (define-key map "\C-\M-p" 'polymode-previous-chunk-same-type)
        ;; chunk manipulation
        (define-key map "\M-k" 'polymode-kill-chunk)
        (define-key map "\M-m" 'polymode-mark-or-extend-chunk)
        (define-key map "\C-t" 'polymode-toggle-chunk-narrowing)
        (define-key map "\M-i" 'polymode-insert-new-chunk)
        ;; backends
        (define-key map "e" 'polymode-export)
        (define-key map "E" 'polymode-set-exporter)
        (define-key map "w" 'polymode-weave)
        (define-key map "W" 'polymode-set-weaver)
        (define-key map "t" 'polymode-tangle)
        (define-key map "T" 'polymode-set-tangler)
        (define-key map "$" 'polymode-show-process-buffer)
        ;; todo: add polymode-goto-process-buffer
        map))
    (define-key map [menu-bar Polymode]
      (cons "Polymode"
            (let ((map (make-sparse-keymap "Polymode")))
              (define-key-after map [next]
                '(menu-item "Next chunk" polymode-next-chunk))
              (define-key-after map [previous]
                '(menu-item "Previous chunk" polymode-previous-chunk))
              (define-key-after map [next-same]
                '(menu-item "Next chunk same type" polymode-next-chunk-same-type))
              (define-key-after map [previous-same]
                '(menu-item "Previous chunk same type" polymode-previous-chunk-same-type))
              (define-key-after map [mark]
                '(menu-item "Mark or extend chunk" polymode-mark-or-extend-chunk))
              (define-key-after map [kill]
                '(menu-item "Kill chunk" polymode-kill-chunk))
              (define-key-after map [insert]
                '(menu-item "Insert new chunk" polymode-insert-new-chunk))
              map)))
    map)
  "The minor mode keymap which is inherited by all polymodes.")

(defalias 'polymode-mode-map 'polymode-minor-mode-map)


;;; COMMANDS
(defvar *span*)
(defun polymode-next-chunk (&optional N)
  "Go N chunks forwards.
Return the number of actually moved over chunks."
  (interactive "p")
  (let* ((sofar 0)
         (back (< N 0))
         (beg (if back (point-min) (point)))
         (end (if back (point) (point-max)))
         (N (if back (- N) N)))
    (condition-case nil
        (pm-map-over-spans
         (lambda ()
           (unless (memq (car *span*) '(head tail))
             (when (>= sofar N)
               (signal 'quit nil))
             (setq sofar (1+ sofar))))
         beg end nil back)
      (quit (when (looking-at "\\s *$")
              (forward-line)))
      (pm-switch-to-buffer))
    sofar))

;;fixme: problme with long chunks .. point is recentered
;;todo: merge into next-chunk
(defun polymode-previous-chunk (&optional N)
  "Go N chunks backwards .
Return the number of chunks jumped over."
  (interactive "p")
  (polymode-next-chunk (- N)))

(defun polymode-next-chunk-same-type (&optional N)
  "Go to next N chunk.
Return the number of chunks of the same type moved over."
  (interactive "p")
  (let* ((sofar 0)
         (back (< N 0))
         (beg (if back (point-min) (point)))
         (end (if back (point) (point-max)))
         (N (if back (- N) N))
         this-type this-name)
    (condition-case nil
        (pm-map-over-spans
         (lambda ()
           (unless (memq (car *span*) '(head tail))
             (when (and (equal this-name
                               (eieio-object-name (car (last *span*))))
                        (eq this-type (car *span*)))
               (setq sofar (1+ sofar)))
             (unless this-name
               (setq this-name (eieio-object-name (car (last *span*)))
                     this-type (car *span*)))
             (when (>= sofar N)
               (signal 'quit nil))))
         beg end nil back)
      (quit (when (looking-at "\\s *$")
              (forward-line)))
      (pm-switch-to-buffer))
    sofar))

(defun polymode-previous-chunk-same-type (&optional N)
  "Go to previous N chunk.
Return the number of chunks of the same type moved over."
  (interactive "p")
  (polymode-next-chunk-same-type (- N)))

(defun pm--kill-span (types)
  (let ((span (pm-innermost-span)))
    (when (memq (car span) types)
      (delete-region (nth 1 span) (nth 2 span)))))

(defun polymode-kill-chunk ()
  "Kill current chunk."
  (interactive)
  (pcase (pm-innermost-span)
    (`(,(or `nil `host) ,beg ,end ,_) (delete-region beg end))
    (`(body ,beg ,_ ,_)
     (goto-char beg)
     (pm--kill-span '(body))
     (pm--kill-span '(head tail))
     (pm--kill-span '(head tail)))
    (`(tail ,beg ,end ,_)
     (if (eq beg (point-min))
         (delete-region beg end)
       (goto-char (1- beg))
       (polymode-kill-chunk)))
    (`(head ,_ ,end ,_)
     (goto-char end)
     (polymode-kill-chunk))
    (_ (error "Canoot find chunk to kill"))))

(defun polymode-toggle-chunk-narrowing ()
  "Toggle narrowing of the current chunk."
  (interactive)
  (if (buffer-narrowed-p)
      (progn (widen) (recenter))
    (pcase (pm-innermost-span)
      (`(head ,_ ,end ,_)
       (goto-char end)
       (pm-narrow-to-span))
      (`(tail ,beg ,_ ,_)
       (if (eq beg (point-min))
           (error "Invalid chunk")
         (goto-char (1- beg))
         (pm-narrow-to-span)))
      (_ (pm-narrow-to-span)))))


(defun polymode-mark-or-extend-chunk ()
  "Not implemented yet."
  (interactive)
  (error "Not implemented yet"))

(defun polymode-insert-new-chunk ()
  "Not implemented yet."
  (interactive)
  (error "Not implemented yet"))

(defun polymode-show-process-buffer ()
  "Show the process buffer used by weaving and exporting programs."
  (interactive)
  (let ((buf (cl-loop for b being the buffers
                      if (buffer-local-value 'pm--process-buffer b)
                      return b)))
    (if buf
        (pop-to-buffer buf `(nil . ((inhibit-same-window . ,pop-up-windows))))
      (message "No polymode process buffers found."))))



;;; DEFINE

(defun pm--config-name (symbol &optional must-exist)
  (let ((config-name
         (if (and (boundp symbol)
                  (symbol-value symbol)
                  (object-of-class-p (symbol-value symbol) 'pm-polymode))
             symbol
           (let ((poly-name (replace-regexp-in-string "poly-\\|-mode\\|-minor-mode" ""
                                                      (symbol-name symbol))))
             (intern (concat "pm-poly/" poly-name))))))
    (when must-exist
      (unless (boundp config-name)
        (error "No pm-polymode config object with name `%s'" config-name))
      (unless (object-of-class-p (symbol-value config-name) 'pm-polymode)
        (error "`%s' is not a `pm-polymode' config object" config-name)))
    config-name))

;;;###autoload
(defmacro define-polymode (mode &optional parent doc &rest body)
  "Define a new polymode MODE.
This macro defines command MODE and an indicator variable MODE
which becomes t when MODE is active and nil otherwise.

MODE command can be used as both major and minor mode. Using
polymodes as minor modes makes sense when :hostmode (see below)
is not specified, in which case polymode installs only inner
modes and doesn't touch current major mode.

Standard hook MODE-hook is run at the end of the initialization
of each polymode buffer (both indirect and base buffers).

This macro also defines the MODE-map keymap from the :keymap
argument and PARENT-map (see below) and pm-poly/[MODE-NAME]
custom variable which holds a `pm-polymode' configuration object
for this polymode.

PARENT is either the polymode configuration object or a polymode
mode (there is 1-to-1 correspondence between config
objects (`pm-polymode') and mode functions). The new polymode
MODE inherits alll the behavior from PARENT except for the
overwrites specified by the keywords (see below). The new MODE
runs all the hooks from the PARENT-mode and inherits its MODE-map
from PARENT-map.

DOC is an optional documentation string. If present PARENT must
be provided, but can be nil.

BODY is executed after the complete initialization of the
polymode but before MODE-hook. It is executed once for each
polymode buffer - host buffer on initialization and every inner
buffer subsequently created.

Before the BODY code keyword arguments (i.e. alternating keywords
and values) are allowed. The following special keywords
controlling the behavior of the new MODE are supported:

:lighter Optional LIGHTER is displayed in the mode line when the
   mode is on. If omitted, it defaults to the :lighter slot of
   CONFIG object.

:keymap If nil, a new MODE-map keymap is created what directly
  inherits from the PARENT's keymap. The last keymap in the
  inheritance chain is always `polymode-minor-mode-map'. If a
  keymap it is used directly as it is. If a list of binding of
  the form (KEY . BINDING) it is merged the bindings are added to
  the newly create keymap.

:after-hook A single form which is evaluated after the mode hooks
  have been run. It should not be quoted.

Other keywords are added to the `pm-polymode' configuration
object and should be valid slots in PARENT config object or the
root config `pm-polymode' object if PARENT is nil. By far the
most frequently used slots are:

:hostmode Symbol pointing to a `pm-host-chunkmode' object
  specifying the behavior of the hostmode. If missing or nil,
  MODE will behave as a minor-mode in the sense that it will
  reuse the currently installed major mode and will install only
  the inner modes.

:innermodes List of symbols pointing to `pm-inner-chunkmode'
  objects which specify the behavior of inner modes (or submodes)."
  (declare
   (doc-string 3)
   (debug (&define name
                   [&optional [&not keywordp] name]
                   [&optional stringp]
                   [&rest [keywordp sexp]]
                   def-body)))

  (if (keywordp parent)
      (progn
        (push doc body)
        (push parent body)
        (setq doc nil
              parent nil))
    (when (keywordp doc)
      (progn
        (push doc body)
        (setq doc nil))))

  (unless (symbolp parent)
    (error "PARENT must be a name of a `pm-polymode' config or a polymode mode function"))

  (let* ((last-message (make-symbol "last-message"))
         (mode-name (symbol-name mode))
         (config-name (pm--config-name mode))
         (root-name (replace-regexp-in-string "poly-\\|-mode" "" mode-name))
         (pretty-name (concat root-name " polymode"))
         (parent-name (and parent (pm--config-name parent t)))
         (parent-conf (and parent-name (symbol-value parent-name)))
         (parent-map nil)
         (keymap-name (intern (concat mode-name "-map")))
         keymap new-innermodes slots after-hook keyw lighter)

    ;; Check keys
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (`:lighter (setq lighter (purecopy (pop body))))
        (`:keymap (setq keymap (pop body)))
        (`:after-hook (setq after-hook (pop body)))
        (`:add-innermodes (setq new-innermodes (pop body)))
        (`:keylist (error ":keylist is not allowed in `define-polymode'"))
        (_ (push (pop body) slots) (push keyw slots))))

    (when new-innermodes
      (if parent
          (let ((new-list (delete-dups
                           (append (eieio-oref parent-conf 'innermodes)
                                   (if (eq (car new-innermodes) 'quote)
                                       (cadr new-innermodes)
                                     new-innermodes)))))
            (setq slots (plist-put slots :innermodes `(quote ,new-list))))
        (error ":add-innermodes needs non-nil PARENT")))

    (unless (keymapp keymap)
      ;; keymap is either nil or list from parent keymap
      (cond
       ;; 1. if parent is config object, merge all list keymaps from parents
       ((and parent
             (eieio-object-p (symbol-value parent)))
        (let ((keylist (copy-sequence keymap))
              (pi parent-conf))
          (while pi
            (let ((map (and (slot-boundp pi :keylist)
                            (eieio-oref pi 'keylist))))
              (when map
                (if (and (symbolp map)
                         (keymapp (symbol-value map)))
                    ;; if one of the parent's :keylist is a keymap, use it as our
                    ;; parent-map and stop further descent
                    (setq parent-map map
                          pi nil)
                  ;; list, descend to next parent and append the key list to keylist
                  (setq pi (and (slot-boundp pi :parent-instance)
                                (eieio-oref pi 'parent-instance))
                        keylist (append map keylist))))))
          (setq keymap (reverse keylist))
          (setq parent-map (or parent-map 'polymode-minor-mode-map))))
       ;; 2. If mode function, take the minor-mode from the parent config
       (parent-conf
        (setq parent-map
              (derived-mode-map-name
               (eieio-oref parent-conf 'minor-mode))))
       ;; 3. nil
       (t (setq parent-map
                'polymode-minor-mode-map))))


    `(progn
       ;; Define the variable to enable or disable the mode.
       (defvar-local ,mode nil ,(format "Non-nil if `%s' polymode is enabled." mode))

       :autoload-end

       ;; define the minor-mode's keymap
       (defvar ,keymap-name
         (easy-mmode-define-keymap ',keymap nil nil (list :inherit ,parent-map))
         ,(format "Keymap for %s." mode-name))

       ,(unless (eq parent config-name)
          `(progn
             (defcustom ,config-name nil
               ,(format "Configuration object for `%s' polymode." mode)
               :group 'polymodes
               :type 'object)
             ;; setting in two steps as defcustom is not re-evaluated on repeated evals
             (setq ,config-name
                   ,(if parent-name
                        `(clone ,parent-name
                                :object-name ,(symbol-name config-name)
                                ,@slots)
                      `(pm-polymode :object-name ,(symbol-name config-name)
                                    ,@slots)))))

       ;; The actual mode function:
       (defun ,mode (&optional arg)
         ,(format "%s\n\n\\{%s}"
                  ;; fixme: add inheretance info here and warning if body is
                  ;; non-nil (like in define-mirror-mode)
                  (or doc (format "Polymode %s." root-name))
                  keymap-name)
         (interactive)
         (let ((,last-message (current-message))
               (state (cond
                       ((numberp arg) (> arg 0))
                       (arg t)
                       ((not ,mode)))))
           (setq ,mode state)
           ;; The 'unless' is needed because inner modes during
           ;; initialization call the same polymode minor-mode which
           ;; triggers this `pm-initialize'.
           (unless (buffer-base-buffer)
             (when ,mode
               (let ((obj (clone ,config-name)))
                 (oset obj :minor-mode ',mode)
                 (pm-initialize obj))
               ;; when host mode is reset in pm-initialize we end up with now
               ;; minor mode in hosts
               (setq ,mode t)))
           ;; body and hooks are executed in all buffers!
           ,@body
           (unless (buffer-base-buffer)
             ;; Avoid overwriting a message shown by the body,
             ;; but do overwrite previous messages.
             (when (and (called-interactively-p 'any)
                        (or (null (current-message))
                            (not (equal ,last-message
                                        (current-message)))))
               (message ,(format "%s enabled" pretty-name))))
           (force-mode-line-update)
           (pm--run-derived-mode-hooks ,config-name)
           ,@(when after-hook `(,after-hook)))
         ;; Return the new state
         ,mode)

       (add-minor-mode ',mode ,(or lighter " PM") ,keymap-name))))

(define-minor-mode polymode-minor-mode
  "Polymode minor mode, used to make everything work."
  nil " PM")

(define-derived-mode poly-head-tail-mode prog-mode "HeadTail"
  "Default major mode for polymode head and tail spans.")

(define-derived-mode poly-fallback-mode prog-mode "FallBack"
  ;; fixme:
  ;; 1. doesn't work as fallback for hostmode
  ;; 2. highlighting is lost (Rnw with inner fallback)
  "Default major mode for modes which were not found.
This is better than fundamental-mode because it allows running
globalized minor modes and can run user hooks.")



;;; FONT-LOCK
;; indulge elisp font-lock :)
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\(define-polymode\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))


(provide 'polymode)
;;; polymode.el ends here
