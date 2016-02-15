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

(require 'polymode-common)
(require 'polymode-classes)
(require 'polymode-methods)
(require 'polymode-compat)
(require 'polymode-export)
(require 'polymode-weave)
(require 'poly-lock)
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


;;; CORE
(defsubst pm-base-buffer ()
  ;; fixme: redundant with :base-buffer 
  "Return base buffer of current buffer, or the current buffer if it's direct."
  (or (buffer-base-buffer (current-buffer))
      (current-buffer)))

(defun pm-get-innermost-span (&optional pos)
  "Get span object at POS."
  (save-restriction
	(widen)
	(let* ((span (pm-get-span pm/polymode pos))
		   (beg (nth 1 span))
		   (end (nth 2 span)))
	  ;; might be used by external applications like flyspell
	  (with-silent-modifications
        (add-text-properties beg end
                             (list :pm-span-type (car span)
                                   :pm-span-beg beg
                                   :pm-span-end end)))
	  span)))

(defun pm-span-to-range (span)
  (and span (cons (nth 1 span) (nth 2 span))))

(defun pm-get-innermost-range (&optional pos)
  (pm-span-to-range (pm-get-innermost-span pos)))

(defun pm-switch-to-buffer (&optional pos)
  "Bring the appropriate polymode buffer to front.
This is done visually for the user with `switch-to-buffer'. All
necessary adjustment like overlay and undo history transport are
performed."
  (let ((span (pm-get-innermost-span pos))
        (pm--select-buffer-visually t))
	(pm-select-buffer (car (last span)) span)))

(defun pm-set-buffer (&optional pos)
  "Set buffer to polymode buffer appropriate for location POS.
This is done with `set-buffer' and no visual adjustments are
done."
  (let ((span (pm-get-innermost-span pos))
        (pm--select-buffer-visually nil))
	(pm-select-buffer (car (last span)) span)))

(defun pm-map-over-spans (fun beg end &optional count backward? visually?)
  "For all spans between BEG and END, execute FUN.
FUN is a function of no args. It is executed with point at the
beginning of the span and with the buffer narrowed to the
span. If COUNT is non-nil, jump at most that many times. If
BACKWARD? is non-nil, map backwards.
 
During the call of FUN, a dynamically bound variable *span* holds
the current innermost span."
  (save-restriction
	(widen)
    (goto-char (if backward? end beg))
	(let ((nr 0)
		  (*span* (pm-get-innermost-span (point))))
      ;; if beg or end coincide with span's limit move to next/previous span
      (if backward?
          (and (eq end (nth 1 *span*))
               (not (bobp))
               (forward-char -1))
        (and (eq beg (nth 2 *span*))
             (not (eobp))
             (forward-char 1)))
	  (while (and (if backward?
					  (> (point) beg)
					(< (point) end))
				  (or (null count)
					  (< nr count)))
		(setq *span* (pm-get-innermost-span)
			  nr (1+ nr))
		(let ((pm--select-buffer-visually visually?))
		  (pm-select-buffer (car (last *span*)) *span*)) ;; object and span
		(goto-char (nth 1 *span*))
		;; (narrow-to-region (nth 1 *span*) (nth 2 *span*))
		(funcall fun)
		;; enter next/previous chunk as head-tails don't include their boundaries
		(if backward?
			(goto-char (max 1 (1- (nth 1 *span*)))) 
		  (goto-char (min (point-max) (1+ (nth 2 *span*)))))))))

(defun pm-narrow-to-span (&optional span)
  "Narrow to current chunk."
  (interactive)
  (unless (= (point-min) (point-max))
    (let ((span (or span
                    (pm-get-innermost-span))))
      (let ((min (nth 1 span))
			(max (nth 2 span)))
		(setq syntax-ppss-last (cons min (list 0 nil min nil nil nil 0 nil nil nil)))
		(narrow-to-region min max)))))

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

(define-derived-mode poly-head-tail-mode prog-mode "HeadTail"
  "Default major mode for polymode head and tail spans.")


;;; FONT-LOCK
;; indulge elisp font-lock :) 
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\(define-polymode\\)\\s +\\(\\(\\w\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))))


;;; TOOLS for DEBUGGING
(defvar pm-allow-post-command-hook t)

(defvar pm--underline-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  '(:underline (:color "red" :style wave)))
    overlay)
  "Overlay used in `pm-debug-mode'.")

(defvar pm--inverse-video-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  '(:inverse-video t))
    overlay)
  "Overlay used by `pm-debug-map-over-spans-and-highlight'.")

(defvar pm-debug-minor-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "M-n M-i") 'pm-debug-info-on-current-span)
	(define-key map (kbd "M-n i") 'pm-debug-info-on-current-span)
    (define-key map (kbd "M-n M-p") 'pm-debug-print-relevant-variables)

    (define-key map (kbd "M-n M-t m") 'pm-debug-toogle-info-message)
	(define-key map (kbd "M-n M-t f") 'pm-debug-toggle-fontification)
	(define-key map (kbd "M-n M-t M-f") 'pm-debug-toggle-fontification)
	(define-key map (kbd "M-n M-t p") 'pm-debug-toggle-post-command)
	(define-key map (kbd "M-n M-t c") 'pm-debug-toggle-after-change)
	(define-key map (kbd "M-n M-t M-c") 'pm-debug-toggle-after-change)
	(define-key map (kbd "M-n M-t a") 'pm-debug-toggle-all)
	(define-key map (kbd "M-n M-t M-a") 'pm-debug-toggle-all)
	(define-key map (kbd "M-n M-t t") 'pm-debug-trace-relevant-functions)
	(define-key map (kbd "M-n M-t M-t") 'pm-debug-trace-relevant-functions)
	(define-key map (kbd "M-n M-t u") 'pm-debug-untrace-relevant-functions)
	(define-key map (kbd "M-n M-t M-u") 'pm-debug-untrace-relevant-functions)
	(define-key map (kbd "M-n M-h") 'pm-debug-map-over-spans-and-highlight)

	(define-key map (kbd "M-n M-f t") 'pm-debug-toggle-fontification)
	(define-key map (kbd "M-n M-f s") 'pm-debug-fontify-current-span)
	(define-key map (kbd "M-n M-f e") 'pm-debug-fontify-last-font-lock-error)
	(define-key map (kbd "M-n M-f h") 'pm-debug-highlight-last-font-lock-error-region)
	(define-key map (kbd "M-n M-f M-t") 'pm-debug-toggle-fontification)
	(define-key map (kbd "M-n M-f M-s") 'pm-debug-fontify-current-span)
	(define-key map (kbd "M-n M-f M-e") 'pm-debug-fontify-last-font-lock-error)
	map))

(define-minor-mode pm-debug-minor-mode
  "Turns on/off useful facilities for debugging polymode.

Key bindings:
\\{pm-debug-minor-mode-map}"
  nil
  " PMDBG"
  :group 'polymode
  (interactive)
  (if pm-debug-minor-mode
	  (progn
		;; this is global hook. No need to complicate with local hooks
		(add-hook 'post-command-hook 'pm-debug-highlight-current-span))
	(delete-overlay pm--underline-overlay)
	(delete-overlay pm--inverse-video-overlay)
	(remove-hook 'post-command-hook 'pm-debug-highlight-current-span)))

(defun pm-debug-minor-mode-on ()
  ;; activating everywhere (in case font-lock infloops in a polymode buffer )
  ;; this doesn't activate in fundamental mode
  (pm-debug-minor-mode t))

(define-globalized-minor-mode pm-debug-mode pm-debug-minor-mode pm-debug-minor-mode-on)

(defun pm-debug-highlight-current-span ()
  (when polymode-mode
	(unless (memq this-command '(pm-debug-info-on-current-span
								 pm-debug-highlight-last-font-lock-error-region))
	  (delete-overlay pm--inverse-video-overlay))
	(condition-case err
		(let ((span (pm-get-innermost-span)))
		  (when pm-debug-display-info-message
			(pm--debug-info span))
		  (move-overlay pm--underline-overlay (nth 1 span) (nth 2 span) (current-buffer)))
	  (error (message "%s" (error-message-string err))))))

(defgeneric pm-debug-info (chunkmode))
(defmethod pm-debug-info (chunkmode)
  (format "class:%s" (eieio-object-class-name chunkmode)))
(defmethod pm-debug-info ((chunkmode pm-hbtchunkmode))
  (format "head-reg:\"%s\" tail-reg:\"%s\" %s" 
		  (oref obj :head-reg) (oref obj :tail-reg)
		  (call-next-method)))
(defmethod pm-debug-info ((chunkmode pm-hbtchunkmode))
  (format "head-reg:\"%s\" tail-reg:\"%s\" %s" 
		  (oref obj :head-reg) (oref obj :tail-reg)
		  (call-next-method)))
(defmethod pm-debug-info ((chunkmode pm-hbtchunkmode-auto))
		  (call-next-method))

(defun pm--debug-info (&optional span)
  (let* ((span (or span (pm-get-innermost-span)))
		 (message-log-max nil)
		 (beg (nth 1 span))
		 (end (nth 2 span))
		 (obj (nth 3 span)))
	(message "(%s) type:%s span:%s-%s %s"
			 major-mode (or (car span) 'host) beg end (pm-debug-info obj))))

(defun pm-debug-info-on-current-span ()
  (interactive)
  (if (not polymode-mode)
	  (message "not in a polymode buffer")
	(let ((span (pm-get-innermost-span)))
	  (pm--debug-info span)
	  (move-overlay pm--inverse-video-overlay (nth 1 span) (nth 2 span) (current-buffer)))))

(defvar pm-debug-display-info-message nil)
(defun pm-debug-toogle-info-message ()
  (interactive)
  (setq pm-debug-display-info-message (not pm-debug-display-info-message)))

(defun pm-debug-toggle-fontification ()
  (interactive)
  (if poly-lock-allow-fontification
	  (progn
		(message "fontificaiton disabled")
		(setq poly-lock-allow-fontification nil))
	(message "fontificaiton enabled")
	(setq poly-lock-allow-fontification t)))

(defun pm-debug-toggle-after-change ()
  (interactive)
  (if poly-lock-allow-after-change
	  (progn
		(message "after-change disabled")
		(setq poly-lock-allow-after-change nil))
	(message "after-change enabled")
	(setq poly-lock-allow-after-change t)))

(defun pm-debug-toggle-post-command ()
  (interactive)
  (if pm-allow-post-command-hook
	  (progn
		(message "post-command disabled")
		(setq pm-allow-post-command-hook nil))
	(message "post-command enabled")
	(setq pm-allow-post-command-hook t)))

(defun pm-debug-toggle-all ()
  (interactive)
  (if poly-lock-allow-fontification
	  (progn
		(message "fontificaiton, after-chnage and command-hook disabled")
		(setq poly-lock-allow-fontification nil
			  poly-lock-allow-after-change nil
			  pm-allow-post-command-hook nil))
	(message "fontificaiton, after-change and command-hook enabled")
	(setq poly-lock-allow-fontification t
		  poly-lock-allow-after-change t
		  pm-allow-post-command-hook t)))

(defun pm-debug-fontify-current-span ()
  (interactive)
  (let ((span (pm-get-innermost-span))
		(poly-lock-allow-fontification t))
	(poly-lock-fontify-region (nth 1 span) (nth 2 span))))

(defun pm-debug-fontify-last-font-lock-error ()
  (interactive)
  (let ((reg (pm--debug-get-last-fl-error))
		(poly-lock-allow-fontification t))
	(if reg
		(progn
		  ;; (pm-debug-blink-region (car reg) (cdr reg) 2)
		  (poly-lock-fontify-region (car reg) (cdr reg)))
	  (message "No last font-lock errors found"))))

(defun pm--debug-get-last-fl-error ()
  (with-current-buffer (messages-buffer)
	 (goto-char (point-max))
	 (when (re-search-backward "(poly-lock-fontify-region \\([0-9]+\\) \\([0-9]+\\))" nil t)
	   (cons (string-to-number (match-string 1))
			 (string-to-number (match-string 2))))))

(defun pm-debug-highlight-last-font-lock-error-region ()
  (interactive)
  (let ((reg (pm--debug-get-last-fl-error)))
	(if reg
		(progn
		  (goto-char (car reg))
		  (recenter)
		  (move-overlay pm--inverse-video-overlay (car reg) (cdr reg) (current-buffer))
		  (message "Region %s" reg))
	  (message "No last font-lock errors found"))))

(defvar pm-debug-relevant-functions-alist
  '((polymode-initialization . (pm-initialize pm--mode-setup pm--common-setup
								pm--create-indirect-buffer pm--get-chunkmode-buffer-create))
	(poly-lock . (poly-lock-mode poly-lock-fontify-region
								 poly-lock-fontification-function
								 poly-lock-after-change
								 poly-lock-refontify
                                 pm--fontify-region-original))
    (jit-loc . (jit-lock-refontify jit-lock-mode jit-lock-fontify-now))
	
	(font-lock . (;; font-lock-mode turn-on-font-lock-if-desired
                  turn-on-font-lock
                  font-lock-after-change-function
                  font-lock-fontify-region font-lock-flush
                  font-lock-fontify-buffer font-lock-ensure))))

(defun pm-debug-trace-background-1 (fn)
  (trace-function-background fn nil
                             '(lambda ()
                                (format " [buf:%s pos:%s type:%s]"
                                        (current-buffer) (point)
                                        (get-text-property (point) :pm-span-type)))))

(defun pm-debug-trace-relevant-functions ()
  (interactive)
  (require 'trace)
  (let* ((groups (append '("*ALL*") (mapcar #'car pm-debug-relevant-functions-alist)))
		 (group-name (completing-read "Trace group: " groups nil t)))
	(if (equal group-name "*ALL*")
		(mapc (lambda (group)
				(mapc #'pm-debug-trace-background-1
                      (assoc group pm-debug-relevant-functions-alist)))
			  (cdr groups))
	  (mapc #'pm-debug-trace-background-1
            (assoc (intern group-name) pm-debug-relevant-functions-alist)))))

(defvar pm-debug-relevant-variables '(fontification-functions
                                      font-lock-flush-function
                                      font-lock-ensure-function
                                      font-lock-fontify-region-function
                                      font-lock-fontify-buffer-function
                                      font-lock-unfontify-region-function
                                      font-lock-unfontify-buffer-function
                                      post-command-hook
                                      indent-line-function))
  
(defun pm-debug-print-relevant-variables ()
  (interactive)
  (let ((buff (get-buffer-create "*polymode-vars*"))
        (vars (mapcar (lambda (v) (cons v (buffer-local-value v (current-buffer))))
                      pm-debug-relevant-variables))
        (cbuff (current-buffer)))
    (require 'pp)
    (with-current-buffer buff
      (goto-char (point-max))
      (insert "===============================================================\n")
      (insert (format "relevant vars in buffer: %s\n" cbuff))
      (insert (pp-to-string vars))
      (toggle-truncate-lines -1))
    (display-buffer buff)))

(defun pm-debug-untrace-relevant-functions ()
  (interactive)
  (require 'trace)
  (let* ((groups (append `("*ALL*") (mapcar #'car pm-debug-relevant-functions-alist)))
		 (group-name (completing-read "Trace group: " groups nil t)))
	(if (equal group-name "*ALL*")
		(mapc (lambda (group)
				(mapc #'untrace-function (assoc group pm-debug-relevant-functions-alist)))
			  (cdr groups))
	  (mapc #'untrace-function (assoc group pm-debug-relevant-functions-alist)))))

(defun pm-debug-blink-region (start end &optional delay)
  (move-overlay pm--inverse-video-overlay start end (current-buffer))
  (run-with-timer (or delay 0.4) nil (lambda () (delete-overlay pm--inverse-video-overlay))))

(defun pm-debug-map-over-spans-and-highlight ()
  (interactive)
  (pm-map-over-spans (lambda ()
                       (let ((start (nth 1 *span*))
                             (end (nth 2 *span*)))
                         (pm-debug-blink-region start end)
                         (sit-for 1)))
                     (point-min) (point-max) nil nil t))

(defun pm--highlight-span (&optional hd-matcher tl-matcher)
  (interactive)
  (let* ((hd-matcher (or hd-matcher (oref pm/chunkmode :head-reg)))
         (tl-matcher (or tl-matcher (oref pm/chunkmode :tail-reg)))
         (span (pm--span-at-point hd-matcher tl-matcher)))
    (pm-debug-blink-region (nth 1 span) (nth 2 span))
    (message "span: %s" span)))

(defun pm-debug-run-over-check ()
  (interactive)
  (goto-char (point-min))
  (let ((start (current-time))
        (count 1))
    (pm-switch-to-buffer)
    (while (< (point) (point-max))
      (setq count (1+ count))
      (forward-char)
      (pm-switch-to-buffer))
    (let ((elapsed  (float-time (time-subtract (current-time) start))))
      (message "elapsed: %s  per-char: %s" elapsed (/ elapsed count)))))

(provide 'polymode)
;;; polymode.el ends here
