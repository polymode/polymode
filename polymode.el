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
;;   Usage: https://github.com/vitoshka/polymode
;;   
;;   Design new polymodes: https://github.com/vitoshka/polymode/tree/master/modes
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'polymode-common)
(require 'polymode-classes)
(require 'polymode-methods)
(require 'polymode-export)
(require 'polymode-weave)
;; load all core polymode and host objects
(require 'poly-base)

(defgroup polymode nil
  "Object oriented framework for multiple modes based on indirect buffers"
  :link '(emacs-commentary-link "polymode")
  :group 'tools)

(defgroup polymodes nil
  "Polymode Configuration Objects"
  :group 'polymode)

(defgroup hostmodes nil
  "Polymode Host Chunkmode Objects"
  :group 'polymode)

(defgroup innermodes nil
  "Polymode Chunkmode Objects"
  :group 'polymode)

(defvar polymode-select-mode-hook nil ;; not used yet
  "Hook run after a different mode is selected.")

(defvar polymode-indirect-buffer-hook nil ;; not used yet
  "Hook run by `pm/install-mode' in each indirect buffer.
It is run after all the indirect buffers have been set up.")

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
        (pm/map-over-spans
         (lambda ()
           (unless (memq (car *span*) '(head tail))
             (when (>= sofar N)
               (signal 'quit nil))
             (setq sofar (1+ sofar))))
         beg end nil back)
      (quit (when (looking-at "\\s *$")
              (forward-line))))
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
        (pm/map-over-spans
         (lambda ()
           (unless (memq (car *span*) '(head tail))
             (when (and (equal this-class
                               (pm--object-name (car (last *span*))))
                        (eq this-type (car *span*)))
               (setq sofar (1+ sofar)))
             (unless this-class
               (setq this-class (pm--object-name (car (last *span*)))
                     this-type (car *span*)))
             (when (>= sofar N)
               (signal 'quit nil))))
         beg end nil back)
      (quit (when (looking-at "\\s *$")
              (forward-line))))
    sofar))

(defun polymode-previous-chunk-same-type (&optional N)
  "Go to previus COUNT chunk.
Return, how many chucks actually jumped over."
  (interactive "p")
  (polymode-next-chunk-same-type (- N)))

(defun pm--kill-span (types)
  (let ((span (pm/get-innermost-span)))
    (when (memq (car span) types)
      (delete-region (nth 1 span) (nth 2 span)))))

(defun polymode-kill-chunk ()
  "Kill current chunk"
  (interactive)
  (pcase (pm/get-innermost-span)
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
    (pcase (pm/get-innermost-span)
      (`(head ,_ ,end ,_)
       (goto-char end)
       (pm/narrow-to-span))
      (`(tail ,beg ,end ,_)
       (if (eq beg (point-min))
           (error "Invalid chunk")
         (goto-char (1- beg))
         (pm/narrow-to-span)))
      (_ (pm/narrow-to-span)))))

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
        (pop-to-buffer buf)
      (message "No polymode process buffers found."))))


;;; CORE
(defsubst pm/base-buffer ()
  ;; fixme: redundant with :base-buffer 
  "Return base buffer of current buffer, or the current buffer if it's direct."
  (or (buffer-base-buffer (current-buffer))
      (current-buffer)))

;; VS[26-08-2012]: Dave's original comment: It would be nice to cache the
;; results of this on text properties, but that probably won't work well if
;; chunks can be nested.  In that case, you can't just mark everything between
;; delimiters -- you have to consider other possible regions between them.  For
;; now, we do the calculation each time, scanning outwards from point.
(defun pm/get-innermost-span (&optional pos)
  (pm-get-span pm/polymode pos))

;; This function is for debug convenience only in order to avoid limited debug
;; context in polymode-select-buffer
(defvar pm--ignore-post-command-hook nil)
(defun pm--sel-buf ()
  (unless pm--ignore-post-command-hook
    (let ((*span* (pm/get-innermost-span))
          (pm--can-move-overlays t))
      (pm-select-buffer (car (last *span*)) *span*))))

(defun polymode-select-buffer ()
  "Select the appropriate (indirect) buffer corresponding to point's context.
This funciton is placed in local post-command hook."
  (condition-case error
      (pm--sel-buf)
    (error (message "polymode error: %s"
                    (error-message-string error)))))

(defun pm/map-over-spans (fun beg end &optional count backward?)
  "For all spans between BEG and END, execute FUN.
FUN is a function of no args. It is executed with point at the
beginning of the span and with the buffer narrowed to the
span. If COUNT is non-nil, jump at most that many times. If
BACKWARD? is non-nil, map backwards.
 
During the call of FUN, a dynamically bound variable *span* holds
the current innermost span."
  (let ((nr 0)
        *span*)
    ;; (save-excursion
    ;;   (save-window-excursion
    (goto-char (if backward? end beg))
    (while (and (if backward?
                    (> (point) beg)
                  (< (point) end))
                (or (null count)
                    (< nr count)))
      (setq *span* (pm/get-innermost-span)
            nr (1+ nr))
      (pm-select-buffer (car (last *span*)) *span*) ;; object and type
      (goto-char (nth 1 *span*))
      (funcall fun)
      (if backward?
          (goto-char (max 1 (1- (nth 1 *span*)))) ;; enter previous chunk
        (goto-char (nth 2 *span*))))));))

(defun pm/narrow-to-span (&optional span)
  "Narrow to current chunk."
  (interactive)
  (unless (= (point-min) (point-max))
    (let ((span (or span
                    (pm/get-innermost-span))))
      (if span
          (let ((min (nth 1 span))
                (max (nth 2 span)))
            (when (boundp 'syntax-ppss-last)
              (setq syntax-ppss-last
                    (cons (point-min)
                          (list 0 nil (point-min) nil nil nil 0 nil nil nil))))
            (narrow-to-region min max))
        (error "No span found")))))

(defun pm/fontify-region (beg end &optional verbose)
  "Polymode font-lock fontification function.
Fontifies chunk-by chunk within the region.
Assigned to `font-lock-fontify-region-function'.

A fontification mechanism should call
`font-lock-fontify-region-function' (`jit-lock-function' does
that). If it does not, the fontification will probably be screwed
in polymode buffers."
  (let* ((buffer-undo-list t)
	 (inhibit-point-motion-hooks t)
         (font-lock-dont-widen t)
         (buff (current-buffer)))
    (with-silent-modifications
      (font-lock-unfontify-region beg end)
      (save-excursion
        (save-window-excursion
          (pm/map-over-spans
           (lambda ()
             (let ((sbeg (nth 1 *span*))
                   (send (nth 2 *span*)))
               (when (and font-lock-mode font-lock-keywords)
                 (when parse-sexp-lookup-properties
                   (pm--comment-region 1 sbeg))
                 (condition-case err
                     (if (oref pm/chunkmode :font-lock-narrow)
                         (save-restriction
                           ;; fixme: optimization oportunity: Cache chunk state
                           ;; in text properties. For big chunks font-lock
                           ;; fontifies it by smaller segments, thus
                           ;; pm/fontify-region is called multiple times per
                           ;; chunk and spans are computed each time.
                           (narrow-to-region sbeg send)
                           (funcall pm--fontify-region-original
                                    (max sbeg beg) (min send end) verbose))
                       (funcall pm--fontify-region-original
                                (max sbeg beg) (min send end) verbose))
                   (error (message "polymode font-lock error: %s (beg: %s end: %s)"
                                   (error-message-string err) beg end)))
                 (when parse-sexp-lookup-properties
                   (pm--uncomment-region 1 sbeg)))
               (pm--adjust-chunk-face sbeg send (pm-get-adjust-face pm/chunkmode))
               ;; might be needed by external applications like flyspell
               ;; fixme: this should be in a more generic place like pm-get-span
               (put-text-property sbeg send 'chunkmode
                                  (object-of-class-p pm/chunkmode 'pm-hbtchunkmode))
               ;; even if failed, set to t to avoid infloop
               (put-text-property beg end 'fontified t)))
           beg end)
          ;; needed to avoid moving last fontified buffer to second place
          (bury-buffer))))))

(defun pm/syntax-begin-function ()
  (goto-char
   (max (cadr (pm/get-innermost-span))
        (if pm--syntax-begin-function-original
            (save-excursion
              (funcall pm--syntax-begin-function-original)
              (point))
          (point-min)))))


;;; DEFINE
;;;###autoload
(defmacro define-polymode (mode config &optional keymap &rest body)
  "Define a new polymode MODE.
This macro defines command MODE and an indicator variable MODE
that is t when MODE is active and nil othervise.

MODE command is similar to standard emacs major modes and it can
be used in `auto-mode-alist'. Standard hook MODE-hook is run at
the end of the initialization of each polymode buffer, indirect
and base alike. Additionally MODE-map is created based on the
CONFIG's :map slot and the value of the :keymap argument; see
below.

CONFIG is a name of a config object representing the mode.

MODE command can also be use as a minor mode. Current major mode
is not reinitialized if it coincides with the :mode slot of
CONFIG object or if the :mode slot is nil.

BODY contains code to be executed after the complete
  initialization of the polymode (`pm-initialize') and before
  running MODE-hook. Before the actual body code, you can write
  keyword arguments, i.e. alternating keywords and values.  The
  following special keywords are supported:

:lighter SPEC   Optional LIGHTER is displayed in the mode line when
                the mode is on. If omitted, it defaults to
                the :lighter slot of CONFIG object.
:keymap MAP	Same as the KEYMAP argument.

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


;;; FONT-LOCK
;; indulge elisp font-lock :) 
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\(define-polymode\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))

(provide 'polymode)
