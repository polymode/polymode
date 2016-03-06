;;; polymode.el --- Versatile multiple modes with extensive literate programming support
;;
;; Filename: polymode.el
;; Author: Spinu Vitalie
;; Maintainer: Spinu Vitalie
;; Copyright (C) 2013-2014, Spinu Vitalie, all rights reserved.
;; Version: 1.0
;; Package-Requires: ((emacs "24"))
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
;;  Extensible, fast, objected-oriented multimode specifically designed for
;;  literate programming. Extensible support for weaving, tangling and export.
;;
;;   Usage: https://github.com/vspinu/polymode
;;
;;   Design new polymodes: https://github.com/vspinu/polymode/tree/master/modes
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode-core)
(require 'polymode-classes)
(require 'polymode-methods)
(require 'polymode-compat)
(require 'polymode-debug)
(require 'polymode-export)
(require 'polymode-weave)
(require 'poly-lock)
(require 'poly-base)

(defcustom polymode-prefix-key "\M-n"
  "Prefix key for the polymode mode keymap.
Not effective after loading the polymode library."
  :group 'polymode
  :type '(choice string vector))

(defvar polymode-mode-map
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
  "The default minor mode keymap that is active in all polymode
  modes.")


;;; COMMANDS
(defvar *span*)
(defun polymode-next-chunk (&optional N)
  "Go COUNT chunks forwards.
Return, how many chucks actually jumped over."
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
  "Go COUNT chunks backwards .
Return, how many chucks actually jumped over."
  (interactive "p")
  (polymode-next-chunk (- N)))

(defun polymode-next-chunk-same-type (&optional N)
  "Go to next COUNT chunk.
Return, how many chucks actually jumped over."
  (interactive "p")
  (let* ((sofar 0)
         (back (< N 0))
         (beg (if back (point-min) (point)))
         (end (if back (point) (point-max)))
         (N (if back (- N) N))
         this-type this-class)
    (condition-case nil
        (pm-map-over-spans
         (lambda ()
           (unless (memq (car *span*) '(head tail))
             (when (and (equal this-class
                               (eieio-object-name (car (last *span*))))
                        (eq this-type (car *span*)))
               (setq sofar (1+ sofar)))
             (unless this-class
               (setq this-class (eieio-object-name (car (last *span*)))
                     this-type (car *span*)))
             (when (>= sofar N)
               (signal 'quit nil))))
         beg end nil back)
      (quit (when (looking-at "\\s *$")
              (forward-line)))
      (pm-switch-to-buffer))
    sofar))

(defun polymode-previous-chunk-same-type (&optional N)
  "Go to previus COUNT chunk.
Return, how many chucks actually jumped over."
  (interactive "p")
  (polymode-next-chunk-same-type (- N)))

(defun pm--kill-span (types)
  (let ((span (pm-get-innermost-span)))
    (when (memq (car span) types)
      (delete-region (nth 1 span) (nth 2 span)))))

(defun polymode-kill-chunk ()
  "Kill current chunk"
  (interactive)
  (pcase (pm-get-innermost-span)
    (`(,(or `nil `host) ,beg ,end ,_) (delete-region beg end))
    (`(body ,beg ,end ,_)
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
    (_ (error "canoot find chunk to kill"))))

(defun polymode-toggle-chunk-narrowing ()
  "Toggle narrowing of the current chunk."
  (interactive)
  (if (buffer-narrowed-p)
      (progn (widen) (recenter))
    (pcase (pm-get-innermost-span)
      (`(head ,_ ,end ,_)
       (goto-char end)
       (pm-narrow-to-span))
      (`(tail ,beg ,end ,_)
       (if (eq beg (point-min))
           (error "Invalid chunk")
         (goto-char (1- beg))
         (pm-narrow-to-span)))
      (_ (pm-narrow-to-span)))))


(defun polymode-mark-or-extend-chunk ()
  (interactive)
  (error "Not implemented yet"))

(defun polymode-insert-new-chunk ()
  (interactive)
  (error "Not implemented yet"))

(defun polymode-show-process-buffer ()
  (interactive)
  (let ((buf (cl-loop for b being the buffers
                      if (buffer-local-value 'pm--process-buffer b)
                      return b)))
    (if buf
    (pop-to-buffer buf `(nil . ((inhibit-same-window . ,pop-up-windows))))
      (message "No polymode process buffers found."))))


;;; HOOKS
;; In addition to these hooks there is poly-lock-after-change which is placed in
;; after-change-functions. See poly-lock.el

(defun polymode-post-command-select-buffer ()
  "Select the appropriate (indirect) buffer corresponding to point's context.
This funciton is placed in local `post-command-hook'."
  (when (and pm-allow-post-command-hook
             polymode-mode
             pm/chunkmode)
    (condition-case err
        (pm-switch-to-buffer)
      (error (message "(pm-switch-to-buffer %s): %s"
                      (point) (error-message-string err))))))

(defun polymode-before-change-setup (beg end)
  "Run `syntax-ppss-flush-cache' in all polymode buffers.
This function is placed in `before-change-functions' hook."
  ;; Modification hooks are run only in current buffer and not in other (base or
  ;; indirect) buffers. Thus some actions like flush of ppss cache must be taken
  ;; care explicitly. We run some safety hooks checks here as well.
  (dolist (buff (oref pm/polymode -buffers))
    ;; The following two checks are unnecessary by poly-lock design, but we are
    ;; checking them here, just in case.
    ;; VS[06-03-2016]: `fontification-functions' probably should be checked as well.
    (when (memq 'font-lock-after-change-function after-change-functions)
      (remove-hook 'after-change-functions 'font-lock-after-change-function t))
    (when (memq 'jit-lock-after-change after-change-functions)
      (remove-hook 'after-change-functions 'jit-lock-after-change t))

    (with-current-buffer buff
      ;; now `syntax-ppss-flush-cache is harmless, but who knows in the future.
      (when (memq 'syntax-ppss-flush-cache before-change-functions)
        (remove-hook 'before-change-functions 'syntax-ppss-flush-cache t))
      (syntax-ppss-flush-cache beg end))))


;;; DEFINE
;;;###autoload
(defmacro define-polymode (mode config &optional keymap &rest body)
  "Define a new polymode MODE.
This macro defines command MODE and an indicator variable MODE
which becomes t when MODE is active and nil otherwise.

MODE command is similar to standard emacs major modes and it can
be used in `auto-mode-alist'. Standard hook MODE-hook is run at
the end of the initialization of each polymode buffer (both
indirect and base buffers). Additionally MODE-map is created
based on the CONFIG's :map slot and the value of the :keymap
argument; see below.

CONFIG is a name of a config object representing the mode.

MODE command can also be use as a minor mode. Current major mode
is not reinitialized if it coincides with the :mode slot of
CONFIG object or if the :mode slot is nil.

BODY contains code to be executed after the complete
  initialization of the polymode (`pm-initialize') and before
  running MODE-hook. Before the BODY code, you can write keyword
  arguments, i.e. alternating keywords and values.  The following
  special keywords are supported:

:lighter SPEC   Optional LIGHTER is displayed in the mode line when
                the mode is on.  If omitted, it defaults to
                the :lighter slot of CONFIG object.

:keymap MAP Same as the KEYMAP argument.

                If nil, a new MODE-map keymap is created what
                directly inherits from the keymap defined by
                the :map slot of CONFIG object. In most cases it
                is a simple map inheriting form
                `polymode-mode-map'. If t or an alist (of
                bindings suitable to be passed to
                `easy-mmode-define-keymap') a keymap MODE-MAP is
                build by mergin this alist with the :map
                specification of the CONFIG object. If a symbol,
                it should be a variable whose value is a
                keymap. No MODE-MAP is automatically created in
                the latter case and :map slot of the CONFIG
                object is ignored.

:after-hook     A single lisp form which is evaluated after the mode hooks
                have been run.  It should not be quoted."
  (declare
   (debug (&define name name
                   [&optional [&not keywordp] sexp]
                   [&rest [keywordp sexp]]
                   def-body)))


  (when (keywordp keymap)
    (push keymap body)
    (setq keymap nil))

  (let* ((last-message (make-symbol "last-message"))
         (mode-name (symbol-name mode))
         (pretty-name (concat
                       (replace-regexp-in-string "poly-\\|-mode" "" mode-name)
                       " polymode"))
         (keymap-sym (intern (concat mode-name "-map")))
         (hook (intern (concat mode-name "-hook")))
     (extra-keywords nil)
     (after-hook nil)
     keyw lighter)

    ;; Check keys.
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
    (`:lighter (setq lighter (purecopy (pop body))))
    (`:keymap (setq keymap (pop body)))
    (`:after-hook (setq after-hook (pop body)))
    (_ (push keyw extra-keywords) (push (pop body) extra-keywords))))

    `(progn
       :autoload-end

       ;; Define the variable to enable or disable the mode.
       (defvar ,mode nil ,(format "Non-nil if %s mode is enabled." pretty-name))
       (make-variable-buffer-local ',mode)

       (let* ((keymap ,keymap)
              (config ',config)
              (lighter (or ,lighter
                           (oref (symbol-value config) :lighter)))
              key-alist)

         (unless (keymapp keymap)
           ;; keymap is either nil or list. Iterate through parents' :map slot
           ;; and gather keys.
           (setq key-alist keymap)
           (let* ((pi (symbol-value config))
                  map mm-name)
             (while pi
               (setq map (and (slot-boundp pi :map)
                              (oref pi :map)))
               (if (and (symbolp map)
                        (keymapp (symbol-value map)))
                   ;; If one of the parent's :map is a keymap, use it as our
                   ;; keymap and stop further descent.
                   (setq keymap  (symbol-value map)
                         pi nil)
                 ;; Descend to next parent and append the key list to key-alist
                 (setq pi (and (slot-boundp pi :parent-instance)
                               (oref pi :parent-instance))
                       key-alist (append key-alist map))))))

         (unless keymap
           ;; If we couldn't figure out the original keymap:
           (setq keymap polymode-mode-map))

         ;; Define the minor-mode keymap:
         (defvar ,keymap-sym
           (easy-mmode-define-keymap key-alist nil nil `(:inherit ,keymap))
           ,(format "Keymap for %s." pretty-name))

         ;; The actual mode function:
         (defun ,mode (&optional arg) ,(format "%s.\n\n\\{%s}" pretty-name keymap-sym)
                (interactive)
                (unless ,mode
                  (let ((,last-message (current-message)))
                    (unless pm/polymode ;; don't reinstall for time being
                      (let ((config (clone ,config)))
                        (oset config :minor-mode ',mode)
                        (pm-initialize config)))
                    ;; set our "minor" mode
                    (setq ,mode t)
                    ,@body
                    (run-hooks ',hook)
                    ;; Avoid overwriting a message shown by the body,
                    ;; but do overwrite previous messages.
                    (when (and (called-interactively-p 'any)
                               (or (null (current-message))
                                   (not (equal ,last-message
                                               (current-message)))))
                      (message ,(format "%s enabled" pretty-name)))
                    ,@(when after-hook `(,after-hook))
                    (force-mode-line-update)))
                ;; Return the new setting.
                ,mode)

         (add-minor-mode ',mode lighter ,keymap-sym)))))

(define-minor-mode polymode-minor-mode
  "Polymode minor mode, used to make everything work."
  nil " PM" polymode-mode-map)

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
