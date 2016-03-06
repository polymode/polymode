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


;;; CORE
(defsubst pm-base-buffer ()
  ;; fixme: redundant with :base-buffer
  "Return base buffer of current buffer, or the current buffer if it's direct."
  (or (buffer-base-buffer (current-buffer))
      (current-buffer)))

(defun pm-get-cached-span (&optional pos)
  "Get cached span at POS"
  (let ((span (get-text-property (or pos (point)) :pm-span)))
    (when span
      (save-restriction
        (widen)
        (let* ((beg (nth 1 span))
               (end (max beg (1- (nth 2 span)))))
          (when (<= end (point-max))
            (and (eq span (get-text-property beg :pm-span))
                 (eq span (get-text-property end :pm-span))
                 span)))))))

(defun pm-get-innermost-span (&optional pos no-cache)
  "Get span object at POS.
If NO-CACHE is non-nil, don't use cache and force re-computation
of the span."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((span (or (and (not no-cache)
                            (pm-get-cached-span pos))
                       (pm-get-span pm/polymode pos)))
             (beg (nth 1 span))
             (end (nth 2 span)))
        ;; might be used by external applications like flyspell
        (with-silent-modifications
          (add-text-properties beg end
                               (list :pm-span span
                                     :pm-span-type (car span)
                                     :pm-span-beg beg
                                     :pm-span-end end)))
        span))))

(defun pm-span-to-range (span)
  (and span (cons (nth 1 span) (nth 2 span))))

(defun pm-get-innermost-range (&optional pos no-cache)
  (pm-span-to-range (pm-get-innermost-span pos no-cache)))

(defun pm-switch-to-buffer (&optional pos-or-span)
  "Bring the appropriate polymode buffer to front.
This is done visually for the user with `switch-to-buffer'. All
necessary adjustment like overlay and undo history transport are
performed."
  (let ((span (if (or (null pos-or-span)
                      (number-or-marker-p pos-or-span))
                  (pm-get-innermost-span pos-or-span)
                pos-or-span))
        (pm--select-buffer-visibly t))
    (pm-select-buffer (car (last span)) span)))

(defun pm-set-buffer (&optional pos-or-span)
  "Set buffer to polymode buffer appropriate for POS-OR-SPAN.
This is done with `set-buffer' and no visual adjustments are
done."
  (let ((span (if (or (null pos-or-span)
                      (number-or-marker-p pos-or-span))
                  (pm-get-innermost-span pos-or-span)
                pos-or-span))
        (pm--select-buffer-visibly nil))
    (pm-select-buffer (car (last span)) span)))

(defun pm-map-over-spans (fun beg end &optional count backwardp visiblyp no-cache)
  "For all spans between BEG and END, execute FUN.
FUN is a function of no args. It is executed with point at the
beginning of the span. Buffer is *not* narrowed to the span. If
COUNT is non-nil, jump at most that many times. If BACKWARDP is
non-nil, map backwards. During the call of FUN, a dynamically
bound variable *span* holds the current innermost span."
  ;; Important! Never forget to save-excursion when calling
  ;; map-overs-spans. Mapping can end different buffer and invalidate whatever
  ;; caller that used your function.
  (save-restriction
    (widen)
    (setq end (min end (point-max)))
    (goto-char (if backwardp end beg))
    (let* ((nr 1)
           (*span* (pm-get-innermost-span (point) no-cache))
           old-span
           moved)
      ;; if beg (end) coincide with span's end (beg) don't process previous (next) span
      (if backwardp
          (and (eq end (nth 1 *span*))
               (setq moved t)
               (not (bobp))
               (forward-char -1))
        (and (eq beg (nth 2 *span*))
             (setq moved t)
             (not (eobp))
             (forward-char 1)))
      (when moved
        (setq *span* (pm-get-innermost-span (point) no-cache)))
      (while (and (if backwardp
                      (> (point) beg)
                    (< (point) end))
                  (or (null count)
                      (< nr count)))
        (let ((pm--select-buffer-visibly visiblyp))
          (pm-select-buffer (car (last *span*)) *span*)) ;; object and span

        ;; FUN might change buffer and invalidate our *span*. How can we
        ;; intelligently check for this? After-change functions have not been
        ;; run yet (or did they?). We can track buffer modification time
        ;; explicitly (can we?)
        (goto-char (nth 1 *span*))
        (save-excursion
          (funcall fun))

        ;; enter next/previous chunk as head-tails don't include their boundaries
        (if backwardp
            (goto-char (max 1 (1- (nth 1 *span*))))
          (goto-char (min (point-max) (1+ (nth 2 *span*)))))

        (setq old-span *span*)
        (setq *span* (pm-get-innermost-span (point) no-cache)
              nr (1+ nr))

        ;; Ensure progress and avoid infloop due to bad regexp or who knows
        ;; what. Move char by char till we get higher/lower span. Cache is not
        ;; used.
        (while (and (not (eobp))
                    (if backwardp
                        (> (nth 2 *span*) (nth 1 old-span))
                      (< (nth 1 *span*) (nth 2 old-span))))
          (forward-char 1)
          (setq *span* (pm-get-innermost-span (point) t)))))))

(defun pm--reset-ppss-last (&optional span-start force)
  "Reset `syntax-ppss-last' cache if it was recorded before SPAN-START.
If SPAN-START is nil, use span at point. If force, reset
regardless of the position `syntax-ppss-last' was recorder at."
  ;; syntax-ppss has its own condition-case for this case, but that means
  ;; throwing an error each time it calls parse-partial-sexp
  (setq span-start (or span-start (car (pm-get-innermost-range))))
  (when (or force
            (and syntax-ppss-last
                 (car syntax-ppss-last)
                 ;; non-strict is intentional (occasionally ppss is screwed)
                 (<= (car syntax-ppss-last) span-start)))
    (setq syntax-ppss-last
          (cons span-start (list 0 nil span-start nil nil nil 0)))))

(defun pm-narrow-to-span (&optional span)
  "Narrow to current chunk."
  (interactive)
  (unless (= (point-min) (point-max))
    (let ((span (or span
                    (pm-get-innermost-span))))
      (let ((sbeg (nth 1 span))
            (send (nth 2 span)))
        (pm--reset-ppss-last sbeg t)
        (narrow-to-region sbeg send)))))

(defmacro pm-with-narrowed-to-span (span &rest body)
  (declare (indent 1) (debug body))
  `(save-restriction
     (pm-narrow-to-span ,span)
     ,@body))

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
