;; polymode-core.el --- Core initialization and utilities for polymode -*- lexical-binding: t -*-
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

(eval-when-compile (require 'cl-lib))

(require 'gv)
(require 'font-lock)
(require 'color)
(require 'polymode-classes)
(require 'format-spec)
(eval-when-compile
  (require 'derived))


;;; ESSENTIAL DECLARATIONS

(defvar *span* nil)
(defvar-local pm/polymode nil)
(defvar-local pm/chunkmode nil)
(defvar-local pm/type nil)
(defvar-local polymode-mode nil
  "Non-nil if current \"mode\" is a polymode.")
(defvar pm--emacs>26 (version<= "26" emacs-version))

;; overwrites
(defvar-local pm--indent-line-function-original nil)
(defvar-local pm--syntax-propertize-function-original nil)

;; silence the compiler
(defvar pm--output-file nil)
(defvar pm--input-buffer nil)
(defvar pm--input-file nil)
(defvar pm--export-spec nil)
(defvar pm--input-not-real nil)
(defvar pm--output-not-real nil)

;; methods api from polymode-methods.el
(declare-function pm-initialize "polymode-methods")
(declare-function pm-get-buffer-create "polymode-methods")
(declare-function pm-get-adjust-face "polymode-methods")
(declare-function pm-get-span "polymode-methods")

;; eieio silence "unknown slot"
;; http://emacs.1067599.n8.nabble.com/Fixing-quot-Unknown-slot-quot-warnings-td419119.html
(eval-when-compile
  (defclass dummy ()
    ((function) (from-to))))

(defun pm-object-name (obj)
  ;; gives warnings on e25,26 but fine in e27
  (with-no-warnings
    (eieio-object-name-string obj)))

;; shields
(defvar pm-allow-after-change-hook t)
(defvar pm-allow-post-command-hook t)
;; We need this during cascaded call-next-method in pm-initialize. -innermodes
;; are initialized after the hostmode setup has taken place. This means that
;; pm-get-span and all the functionality that relies on it will fail to work
;; correctly during the initialization in the call-next-method. This is
;; particularly relevant to font-lock setup and user hooks.
(defvar pm-initialization-in-progress nil)


;; CUSTOM

(defgroup polymode nil
  "Object oriented framework for multiple modes based on indirect buffers"
  :link '(emacs-commentary-link "polymode")
  :group 'tools)

(defgroup poly-modes nil
  "Polymode Configuration Objects"
  :group 'polymode)

(defgroup poly-host-modes nil
  "Polymode Host Chunkmode Objects"
  :group 'polymode)

(defgroup poly-inner-modes nil
  "Polymode Chunkmode Objects"
  :group 'polymode)

(defcustom polymode-display-process-buffers t
  "When non-nil, display weaving and exporting process buffers."
  :group 'polymode
  :type 'boolean)

(defcustom polymode-skip-processing-when-unmodified t
  "If non-nil, consider modification times of input and output files.
Skip weaving or exporting process when output file is more recent
than the input file."
  :group 'polymode
  :type 'boolean)

(define-obsolete-variable-alias 'polymode-mode-name-override-alist 'polymode-mode-name-alias-alist "2018-08")
(defcustom polymode-mode-name-alias-alist
  '((elisp . emacs-lisp) (el . emacs-lisp)
    (bash . sh-mode))
  "An alist of inner mode overrides.
When inner mode is automatically detected from the header of the
inner chunk (such as in markdown mode), the detected symbol might
not correspond to the desired mode. This alist maps discovered
symbols into desired modes. For example

  (add-to-list 'polymode-mode-name-override-alist '(julia . ess-julia))

will cause installation of `ess-julia-mode' in markdown ```julia chunks."
  :group 'polymode
  :type 'alist)

(defvar polymode-switch-buffer-hook nil
  "Hook run on switching to a different buffer.
Each function is run with two arguments `old-buffer' and
`new-buffer'. This hook is commonly used to transfer state
between buffers. The hook is run in a new buffer, but you should
not rely on that. Slot :switch-buffer-functions in `pm-polymode'
and `pm-chunkmode' objects provides same functionality for
narrower scope.")

(defvar polymode-init-host-hook nil
  "Hook run on initialization of every hostmode.
Ran in a base buffer from `pm-initialze'
methods. Slot :init-functions in `pm-polymode' objects provides
similar hook for more focused scope. See
`polymode-init-inner-hook' and :init-functions slot in
`pm-chunkmode' objects for similar hooks for inner chunkmodes.")

(defvar polymode-init-inner-hook nil
  "Hook run on initialization of every `pm-chunkmode' object.
The hook is run in chunkmode's body buffer from `pm-initialze'
`pm-chunkmode' methods. Slot :init-functions `pm-chunkmode'
objects provides same functionality for narrower scope. See also
`polymode-init-host-hook'.")


;;; MESSAGES

(defvar pm-verbose nil)
(defvar pm-syntax-verbose nil)
(defvar pm-extra-span-info nil)

(defun pm-format-span (&optional span prefixp)
  (let* ((span (cond
                ((number-or-marker-p span) (pm-innermost-span span))
                ((null span) (pm-innermost-span))
                (span)))
         (message-log-max nil)
         (beg (nth 1 span))
         (end (nth 2 span))
         (type (and span (or (car span) 'host)))
         (oname (if span
                    (eieio-object-name (nth 3 span))
                  (current-buffer)))
         (extra (if pm-extra-span-info
                    (format (if prefixp "%s " " (%s)") pm-extra-span-info)
                  "")))
    (if prefixp
        (format "%s[%s %d-%d %s]" extra type beg end oname)
      (format "[%s %d-%d %s]%s" type beg end oname extra))))


;;; SPANS

(defsubst pm-base-buffer ()
  "Return base buffer of current buffer, or the current buffer if it's direct."
  (or (buffer-base-buffer (current-buffer))
      (current-buffer)))

(defun pm-cache-span (span)
  ;; cache span
  (unless pm-initialization-in-progress
    (with-silent-modifications
      ;; (message "caching: %s %s" (car span) (pm-span-to-range span))
      (let ((sbeg (nth 1 span))
            (send (nth 2 span)))
        (put-text-property sbeg send :pm-span span)))))

(defun pm-flush-span-cache (beg end &optional buffer)
  (with-silent-modifications
    (remove-list-of-text-properties beg end '(:pm-span) buffer)))

(defun pm--intersect-spans (config &optional pos)
  ;; fixme: host should be last, to take advantage of the chunkmodes computation?
  (let* ((start (point-min))
         (end (point-max))
         (pos (or pos (point)))
         (span (list nil start end nil))
         (chunk-modes (cons (oref config -hostmode)
                            (oref config -innermodes)))
         val)
    (dolist (im chunk-modes)
      ;; Optimization opportunity: this searches till the end of buffer but the
      ;; outermost pm-get-span caller has computed a few span already so we can
      ;; pass limits or narrow to pre-computed span.
      (setq val (pm-get-span im pos))
      ;; (message "[%d] span: %S imode: %s" (point) (pm-span-to-range span) (pm-debug-info im))
      (when val
        (cond
         ;; 1. ;; nil car means host and it can be an intersection of spans returned
         ;; by 2 different neighbour inner chunkmodes
         ((null (car val))
          (setq start (max (nth 1 val)
                           (nth 1 span))
                end (min (nth 2 val)
                         (nth 2 span)))
          (setcar (cdr span) start)
          (setcar (cddr span) end))
         ;; 2. Inner span
         ((or (> (nth 1 val) start)
              (< (nth 2 val) end))
          (when (or (null (car span))
                    (eieio-oref (nth 3 val) 'can-nest))
            (setq span val
                  start (nth 1 val)
                  end (nth 2 val))))
         ;; 3. Outer span; overwrite previous span if nesting is not allowed.
         ;; This case can probably result in unexpected outcome when there are 3
         ;; levels of nesting with inter-changeable :can-nest property.
         ((and (car span)
               (not (eieio-oref (nth 3 span) 'can-nest)))
          (setq span val
                start (nth 1 val)
                end (nth 2 val))))))

    (unless (and (<= start end) (<= pos end) (>= pos start))
      (error "Bad polymode selection: span:%s pos:%s"
             (list start end) pos))

    (when (null (car span)) ; chunkmodes can compute the host span by returning nil span type
      (setcar (last span) (oref config -hostmode)))

    (pm-cache-span span)

    span))

(defun pm--chop-span (span beg end)
  ;; destructive!
  (when (> beg (nth 1 span))
    (setcar (cdr span) beg))
  (when (< end (nth 2 span))
    (setcar (cddr span) end))
  span)

(defun pm--innermost-span (config &optional pos)
  (let ((pos (or pos (point)))
        (omin (point-min))
        (omax (point-max)))
    (save-excursion
      (save-restriction
        (widen)
        (let ((span (pm--intersect-spans config pos)))
          (if (= omax pos)
              (when (and (= omax (nth 1 span))
                         (> omax omin))
                ;; When pos == point-max and it's beg of span, return the
                ;; previous span. This occurs because the computation of
                ;; pm--intersect-spans is done on a widened buffer.
                (setq span (pm--intersect-spans config (1- pos))))
            (when (= pos (nth 2 span))
              (error "Span ends at %d in (pm-inermost-span %d) %s"
                     pos pos (pm-format-span span))))
          (pm--chop-span span omin omax))))))

(defun pm--cached-span (&optional pos)
  ;; fixme: add basic miss statistics
  (unless pm-initialization-in-progress
    (let* ((omin (point-min))
           (omax (point-max))
           (pos (or pos (point)))
           (pos (if (= pos omax)
                    (max (point-min) (1- pos))
                  pos))
           (span (get-text-property pos :pm-span)))
      (when span
        (save-restriction
          (widen)
          (let* ((beg (nth 1 span))
                 (end (max beg (1- (nth 2 span)))))
            (when (and (< end (point-max)) ; buffer size might have changed
                       (eq span (get-text-property beg :pm-span))
                       (eq span (get-text-property end :pm-span))
                       (not (eq span (get-text-property (1+ end) :pm-span)))
                       (or (= beg (point-min))
                           (not (eq span (get-text-property (1- beg) :pm-span)))))
              (pm--chop-span (copy-sequence span) omin omax))))))))

(define-obsolete-function-alias 'pm-get-innermost-span 'pm-innermost-span "2018-08")
(defun pm-innermost-span (&optional pos no-cache)
  "Get span object at POS.
If NO-CACHE is non-nil, don't use cache and force re-computation
of the span. Return a cons (type start end chunkmode). POS
defaults to point. Guarantied to return a non-empty span."
  (when (and pos (or (< pos (point-min)) (> pos (point-max))))
    (signal 'args-out-of-range
            (list :pos pos
                  :point-min (point-min)
                  :point-max (point-max))))
  (or (unless no-cache
        (pm--cached-span pos))
      (let (;; `re-search-forward' and other search functions trigger full
            ;; `internal--syntax-propertize' on the whole buffer on every
            ;; single buffer modification. This is a small price to pay for a
            ;; much improved efficiency in modes which heavily rely on
            ;; `syntax-propertize' like `markdown-mode'.
            (parse-sexp-lookup-properties nil))
        (pm--innermost-span pm/polymode pos))))

(defun pm-span-to-range (span)
  (and span (cons (nth 1 span) (nth 2 span))))

(define-obsolete-function-alias 'pm-get-innermost-range 'pm-innermost-range "2018-08")
(defun pm-innermost-range (&optional pos no-cache)
  (pm-span-to-range (pm-innermost-span pos no-cache)))

(defun pm-fun-matcher (matcher)
  "Make a function matcher given a MATCHER.
MATCHER is one of the forms accepted by \=`pm-inner-chunkmode''s
:head-matcher slot."
  (cond
   ((stringp matcher)
    (lambda (ahead)
      (if (< ahead 0)
          (if (re-search-backward matcher nil t)
              (cons (match-beginning 0) (match-end 0)))
        (if (re-search-forward matcher nil t)
            (cons (match-beginning 0) (match-end 0))))))
   ((functionp matcher)
    matcher)
   ((consp matcher)
    (lambda (ahead)
      (when (re-search-forward (car matcher) nil t ahead)
        (cons (match-beginning (cdr matcher))
              (match-end (cdr matcher))))))
   (t (error "Head and tail matchers must be either regexp strings, cons cells or functions"))))

(defun pm-same-indent-tail-matcher (_arg)
  "Return the end position of block with the higher indent as the current line.
Used as tail matcher for blocks identified by same indent. See
function `poly-slim-mode' for examples. ARG is ignored; always search
forward."
  (let ((block-col (current-indentation))
        (end (point-at-eol)))
    (forward-line 1)
    (while (and (not (eobp))
                (> (current-indentation) block-col)
                (setq end (point-at-eol)))
      (forward-line 1))
    ;; end at bol for the sake of indentation
    (setq end (min (point-max) (1+ end)))
    (cons end end)))

(defun pm-true-span-type (chunkmode &optional type)
  "Retrieve the TYPE of buffer to be installed for CHUNKMODE.
`pm-innermost-span' returns a raw type (head, body or tail) but
the actual type installed depends on the values of :host-mode ant
:tail-mode of the CHUNKMODE object. Always return nil if TYPE is
nil (aka a host span). CHUNKMODE could also be a span, in which
case TYPE is ignored."
  ;; fixme: this works on inner modes only. Fix naming.
  (when (listp chunkmode)
    ;; a span
    (setq type (car chunkmode)
          chunkmode (nth 3 chunkmode)))
  (unless (or (null type) (eq type 'host))
    (with-slots (mode head-mode tail-mode) chunkmode
      (cond ((or (eq type 'body)
                 (and (eq type 'head)
                      (eq head-mode 'body))
                 (and (eq type 'tail)
                      (or (eq tail-mode 'body)
                          (and (null tail-mode)
                               (eq head-mode 'body)))))
             'body)
            ((or (and (eq type 'head)
                      (eq head-mode 'host))
                 (and (eq type 'tail)
                      (or (eq tail-mode 'host)
                          (and (null tail-mode)
                               (eq head-mode 'host)))))
             nil)
            ((eq type 'head)
             'head)
            ((eq type 'tail)
             (if tail-mode
                 'tail
               'head))
            (t (error "Type must be one of nil, 'host, 'head, 'tail or 'body"))))))

;; return nil if type is nil or 'host
(defun pm--get-chunkmode-mode (chunkmode type)
  (let ((ttype (pm-true-span-type chunkmode type)))
    (cond
     ((eq ttype 'body)
      (eieio-oref chunkmode 'mode))
     ((eq ttype 'head)
      (eieio-oref chunkmode 'head-mode))
     ((eq ttype 'tail)
      (eieio-oref chunkmode 'tail-mode)))))

;; Attempt to check for in-comments; doesn't work because we end up calling
;; syntax-propertize recursively.
;; (defun pm--funcall-matcher (matcher arg)
;;   "Call MATCHER with ARG till a match outside comment has been found."
;;   (let ((beg.end (funcall matcher arg)))
;;     (while (and beg.end
;;                 (> (car beg.end) (point-min))
;;                 (nth 4 (syntax-ppss (1- (car beg.end))))
;;                 (setq beg.end (funcall matcher arg))))
;;     beg.end))

(defun pm--span-at-point (head-matcher tail-matcher &optional pos can-overlap)
  "Span detector with head and tail matchers.
HEAD-MATCHER and TAIL-MATCHER is as in :head-matcher slot of
`pm-inner-chunkmode' object. POS defaults to (point). When
CAN-OVERLAP is non-nil nested chunks of this type are allowed.

Return a list of the form (TYPE SPAN-START SPAN-END) where TYPE
is one of the following symbols:
  nil   - pos is between ‘point-min’ and head-matcher, or between
          tail-matcher and ‘point-max’
  body  - pos is between head-matcher and tail-matcher (exclusively)
  head  - head span
  tail  - tail span"
  (setq pos (or pos (point)))
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (let* ((at-max (= pos (point-max)))
             (head-matcher (pm-fun-matcher head-matcher))
             (tail-matcher (pm-fun-matcher tail-matcher))
             (head1 (funcall head-matcher -1)))
        (if head1
            (if (and at-max (= (cdr head1) pos))
                ;;           |
                ;; host)[head)           ; can occur with sub-head == 0 only
                (list 'head (car head1) (cdr head1))
              ;;            ------------------------
              ;; host)[head)[body)[tail)[host)[head)[body)
              (pm--find-tail-from-head pos head1 head-matcher tail-matcher can-overlap))
          ;; ----------
          ;; host)[head)[body)[tail)[host
          (goto-char (point-min))
          (let ((head2 (funcall head-matcher 1)))
            (if head2
                (if (< pos (car head2))
                    ;; ----
                    ;; host)[head)[body)[tail)[host
                    (list nil (point-min) (car head2))
                  (if (< pos (cdr head2))
                      ;;      -----
                      ;; host)[head)[body)[tail)[host
                      (list 'head (car head2) (cdr head2))
                    ;;            -----------------
                    ;; host)[head)[body)[tail)[host
                    (pm--find-tail-from-head pos head2 head-matcher tail-matcher can-overlap)))
              ;; no span found
              nil)))))))

;; fixme: find a simpler way with recursion where head-matcher and tail-matcher could be reversed
(defun pm--find-tail-from-head (pos head head-matcher tail-matcher can-overlap)
  (goto-char (cdr head))
  (let ((tail (funcall tail-matcher 1))
        (at-max (= pos (point-max)))
        (type 'tail))
    (when can-overlap
      (save-excursion
        ;; search for next head and pick the earliest
        (goto-char (cdr head))
        (let ((match (funcall head-matcher 1)))
          (when (or (null tail)
                    (and match (< (car match) (car tail))))
            (setq tail match
                  type 'head)))))
    (if tail
        (if (< pos (car tail))
            ;;            -----
            ;; host)[head)[body)[tail)[host)[head)
            (list 'body (cdr head) (car tail))
          (if (or (< pos (cdr tail))
                  (and at-max (= pos (cdr tail))))
              ;;                  -----
              ;; host)[head)[body)[tail)[host)[head)
              (list type (car tail) (cdr tail))
            (goto-char (cdr tail))
            ;;                        -----------
            ;; host)[head)[body)[tail)[host)[head)
            (let ((match (funcall head-matcher 1))
                  (type 'head))
              (when can-overlap
                (save-excursion
                  ;; search for next head and pick the earliest
                  (goto-char (cdr tail))
                  (let ((match2 (funcall tail-matcher 1)))
                    (when (or (null match)
                              (and match2 (< (car match2) (car match))))
                      (setq match match2
                            type 'tail)))))
              (if match
                  (if (< pos (car match))
                      ;;                        -----
                      ;; host)[head)[body)[tail)[host)[head)
                      (list nil (cdr tail) (car match))
                    (if (or (< pos (cdr match))
                            (and at-max (= pos (cdr match))))
                        ;;                              -----
                        ;; host)[head)[body)[tail)[host)[head)[body
                        (list type (car match) (cdr match))
                      ;;                                    ----
                      ;; host)[head)[body)[tail)[host)[head)[body
                      (pm--find-tail-from-head pos match head-matcher tail-matcher can-overlap)))
                ;;                        -----
                ;; host)[head)[body)[tail)[host)
                (list nil (cdr tail) (point-max))))))
      ;;            -----
      ;; host)[head)[body)
      (list 'body (cdr head) (point-max)))))


;;; OBJECT HOOKS

(defun pm--run-derived-mode-hooks (config)
  ;; Minor modes run-hooks, major-modes run-mode-hooks.
  ;; Polymodes is a minor mode but with major-mode flavor. We
  ;; run all parent hooks in reversed order here.
  (let ((this-mode (eieio-oref config 'minor-mode)))
    (mapc (lambda (mm)
            (let ((old-mm (symbol-value mm)))
              (unwind-protect
                  (progn
                    (set mm (symbol-value this-mode))
                    (run-hooks (derived-mode-hook-name mm)))
                (set mm old-mm))))
          (pm--collect-parent-slots config :minor-mode))))

(defun pm--run-init-hooks (object &optional emacs-hook)
  (unless pm-initialization-in-progress
    (pm--run-hooks object :init-functions)
    (when emacs-hook
      (run-hooks emacs-hook))))

(defun pm--collect-parent-slots (object slot)
  "Descend into parents of OBJECT and return a list of SLOT values.
Returned list is in parent first order."
  (let ((inst object)
        (vals nil))
    (while inst
      (when (slot-boundp inst slot)
        (push (eieio-oref inst slot) vals))
      (setq inst (and (slot-boundp inst :parent-instance)
                      (eieio-oref inst 'parent-instance))))
    vals))

(defun pm--run-hooks (object slot &rest args)
  "Run hooks from SLOT of OBJECT and its parent instances.
Parents' hooks are run first."
  (let ((funs (delete-dups
               (copy-sequence
                (apply #'append
                       (pm--collect-parent-slots object slot))))))
    (if args
        (mapc (lambda (hook)
                (apply #'run-hook-with-args hook args))
              funs)
      (mapc #'run-hooks funs))))


;;; BUFFER SELECTION

;; Transfer of the buffer-undo-list is managed internally by emacs
(defvar pm-move-vars-from-base '(buffer-file-name)
  "Variables transferred from base buffer on buffer switch.")

(defvar pm-move-vars-from-old-buffer
  '(buffer-undo-list
    buffer-invisibility-spec
    selective-display
    overwrite-mode
    truncate-lines
    word-wrap
    line-move-visual
    truncate-partial-width-windows)
  "Variables transferred from old buffer on buffer switch.")

(defun pm-select-buffer (span &optional visibly)
  "Select the buffer associated with SPAN.
Install a new indirect buffer if it is not already installed.
Chunkmode's class should define `pm-get-buffer-create' method. If
VISIBLY is non-nil perform extra adjustment for \"visual\" buffer
switch."
  (let* ((chunkmode (nth 3 span))
         (type (pm-true-span-type span))
         (buff (if type
                   (pm-get-buffer-create chunkmode type)
                 (pm-get-buffer-create (oref pm/polymode -hostmode)))))
    (pm--select-existing-buffer buff span visibly)))

;; extracted for debugging purpose
(defun pm--select-existing-buffer (buffer span visibly)
  ;; emacs bug: ppss cache is invalidated incorrectly; so need to do this every
  ;; single command
  (with-current-buffer buffer
    ;; (message (pm--debug-info span))
    (pm--reset-ppss-last span))

  ;; (message "setting buffer %d-%d [%s]" (nth 1 span) (nth 2 span) (current-buffer))
  ;; no action if BUFFER is already the current buffer
  (when (and (not (eq buffer (current-buffer)))
             (buffer-live-p buffer))

    (let ((base (pm-base-buffer)))
      (pm--move-vars pm-move-vars-from-old-buffer (current-buffer) buffer)
      (pm--move-vars pm-move-vars-from-base base buffer))

    (if visibly
        ;; slow, visual selection
        (pm--select-existent-buffer-visibly buffer)
      ;; fast set-buffer
      (set-buffer buffer))))

(defvar text-scale-mode)
(defvar text-scale-mode-amount)
(defun pm--select-existent-buffer-visibly (new-buffer)
  (let ((old-buffer (current-buffer))
        (point (point))
        (window-start (window-start))
        (visible (pos-visible-in-window-p))
        (vlm visual-line-mode)
        (ractive (region-active-p))
        ;; text-scale-mode
        (scale (and (boundp 'text-scale-mode) text-scale-mode))
        (scale-amount (and (boundp 'text-scale-mode-amount) text-scale-mode-amount))
        (hl-line (and (boundp 'hl-line-mode) hl-line-mode))
        (mkt (mark t))
        (bro buffer-read-only))

    (when hl-line
      (hl-line-mode -1))

    (pm--move-overlays old-buffer new-buffer)

    (switch-to-buffer new-buffer)
    (bury-buffer-internal old-buffer)

    (unless (eq bro buffer-read-only)
      (read-only-mode (if bro 1 -1)))
    (pm--adjust-visual-line-mode vlm)

    (when (and (boundp 'text-scale-mode-amount)
               (not (and (eq scale text-scale-mode)
                         (= scale-amount text-scale-mode-amount))))
      (if scale
          (text-scale-set scale-amount)
        (text-scale-set 0)))

    ;; fixme: what is the right way to do this ... activate-mark-hook?
    (if (not ractive)
        (deactivate-mark)
      (set-mark mkt)
      (activate-mark))

    ;; avoid display jumps
    (goto-char point)
    (when visible
      (set-window-start (get-buffer-window new-buffer t) window-start))

    (when hl-line
      (hl-line-mode 1))

    (run-hook-with-args 'polymode-switch-buffer-hook old-buffer new-buffer)
    (pm--run-hooks pm/polymode :switch-buffer-functions old-buffer new-buffer)
    (pm--run-hooks pm/chunkmode :switch-buffer-functions old-buffer new-buffer)))

(defun pm--move-overlays (from-buffer to-buffer)
  (with-current-buffer from-buffer
    (mapc (lambda (o)
            (unless (eq 'linum-str (car (overlay-properties o)))
              (move-overlay o (overlay-start o) (overlay-end o) to-buffer)))
          (overlays-in 1 (1+ (buffer-size))))))

(defun pm--move-vars (vars from-buffer &optional to-buffer)
  (let ((to-buffer (or to-buffer (current-buffer))))
    (unless (eq to-buffer from-buffer)
      (with-current-buffer to-buffer
        (dolist (var vars)
          (and (boundp var)
               (set var (buffer-local-value var from-buffer))))))))

(defun pm--adjust-visual-line-mode (vlm)
  (unless (eq visual-line-mode vlm)
    (if (null vlm)
        (visual-line-mode -1)
      (visual-line-mode 1))))

(defun pm-set-buffer (&optional pos-or-span)
  "Set buffer to polymode buffer appropriate for POS-OR-SPAN.
This is done with `set-buffer' and no visual adjustments (like
overlay transport) are done. See `pm-switch-to-buffer' for a more
comprehensive alternative."
  (let ((span (if (or (null pos-or-span)
                      (number-or-marker-p pos-or-span))
                  (pm-innermost-span pos-or-span)
                pos-or-span)))
    (pm-select-buffer span)))

(defun pm-switch-to-buffer (&optional pos-or-span)
  "Bring the appropriate polymode buffer to front.
POS-OR-SPAN can be either a position in a buffer or a span. All
expensive adjustment for a visible switch (like overlay
transport) are performed."
  (let ((span (if (or (null pos-or-span)
                      (number-or-marker-p pos-or-span))
                  (pm-innermost-span pos-or-span)
                pos-or-span)))
    (pm-select-buffer span 'visibly)))

(defun pm-map-over-spans (fun &optional beg end count backwardp visibly no-cache)
  "For all spans between BEG and END, execute FUN.
FUN is a function of no args. It is executed with point at the
beginning of the span. Buffer is *not* narrowed to the span. If
COUNT is non-nil, jump at most that many times. If BACKWARDP is
non-nil, map backwards. During the call of FUN, a dynamically
bound variable *span* holds the current innermost span."
  ;; Important! Don't forget to save-excursion when calling map-overs-spans.
  ;; Mapping can end different buffer and invalidate whatever caller that used
  ;; your function.
  (save-restriction
    (widen)
    (setq beg (or beg (point-min))
          end (if end
                  (min end (point-max))
                (point-max)))
    (unless count
      (setq count most-positive-fixnum))
    (let* ((nr 0)
           (pos (if backwardp end beg))
           (*span* (pm-innermost-span pos no-cache)))
      (while *span*
        (setq nr (1+ nr))
        (pm-select-buffer *span* visibly)
        ;; FUN might change buffer and invalidate our *span*. Should we care or
        ;; reserve pm-map-over-spans for "read-only" actions only? Does
        ;; after-change runs immediately or after this function ends?
        (goto-char (nth 1 *span*))
        (save-excursion
          ;; FIXME: call with *span* argument
          (funcall fun))
        ;; enter previous/next chunk
        (if backwardp
            (goto-char (max 1 (1- (nth 1 *span*))))
          (goto-char (min (point-max) (nth 2 *span*))))
        (setq *span*
              (and (if backwardp
                       (> (point) beg)
                     (< (point) end))
                   (< nr count)
                   (pm-innermost-span (point) no-cache)))))))

(defun pm-narrow-to-span (&optional span)
  "Narrow to current SPAN."
  (interactive)
  (unless (= (point-min) (point-max))
    (let ((span (or span
                    (pm-innermost-span))))
      (let ((sbeg (nth 1 span))
            (send (nth 2 span)))
        (unless pm--emacs>26
          (pm--reset-ppss-last span))
        (narrow-to-region sbeg send)))))

(defmacro pm-with-narrowed-to-span (span &rest body)
  (declare (indent 1) (debug body))
  `(save-restriction
     (pm-narrow-to-span ,span)
     ,@body))



;;; HOOKS
;; In addition to these hooks there is `poly-lock-after-change' in poly-lock.el

(defun polymode-post-command-select-buffer ()
  "Select the appropriate (indirect) buffer corresponding to point's context.
This funciton is placed in local `post-command-hook'."
  (when (and pm-allow-post-command-hook
             polymode-mode
             pm/chunkmode)
    (when pm-syntax-verbose
      (dolist (b (oref pm/polymode -buffers))
        (with-current-buffer b
          (message "sp--done: %d [%s]" syntax-propertize--done (current-buffer)))))
    (condition-case err
        (pm-switch-to-buffer)
      (error (message "(pm-switch-to-buffer %s): %s"
                      (point) (error-message-string err))))))

(defun polymode-before-change-setup (beg end)
  "Run `syntax-ppss-flush-cache' from BEG to END in all polymode buffers.
This function is placed in `before-change-functions' hook."
  ;; Modification hooks are run only in current buffer and not in other (base or
  ;; indirect) buffers. Thus some actions like flush of ppss cache must be taken
  ;; care explicitly. We run some safety hooks checks here as well.
  (dolist (buff (oref pm/polymode -buffers))
    (with-current-buffer buff
      ;; now `syntax-ppss-flush-cache is harmless, but who knows in the future.
      (when (memq 'syntax-ppss-flush-cache before-change-functions)
        (remove-hook 'before-change-functions 'syntax-ppss-flush-cache t))
      (syntax-ppss-flush-cache beg end)
      ;; Check if something has changed our hooks. (Am I theoretically paranoid or
      ;; this is indeed needed?) `fontification-functions' (and others?) should be
      ;; checked as well I guess.
      ;; (when (memq 'font-lock-after-change-function after-change-functions)
      ;;   (remove-hook 'after-change-functions 'font-lock-after-change-function t))
      ;; (when (memq 'jit-lock-after-change after-change-functions)
      ;;   (remove-hook 'after-change-functions 'jit-lock-after-change t))
      )))

(defvar-local pm--killed nil)
(defun polymode-after-kill-fixes ()
  "Various fixes for polymode indirect buffers."
  (when pm/polymode
    (let ((base (pm-base-buffer)))
      (set-buffer-modified-p nil)
      ;; Prevent various tools like `find-file' to re-find this file. We use
      ;; buffer-list instead of `-buffers' slot here because on some occasions
      ;; there are other indirect buffers (e.g. switch from polymode to other
      ;; mode and then back , or when user creates an indirect buffer manually).
      (dolist (b (buffer-list))
        (when (and (buffer-live-p b)
                   (eq (buffer-base-buffer b) base))
          (with-current-buffer b
            (setq pm--killed t)
            (setq buffer-file-name nil)
            (setq buffer-file-number nil)
            (setq buffer-file-truename nil)))))))

(defun polymode-with-current-base-buffer (orig-fun &rest args)
  "Switch to base buffer and apply ORIG-FUN to ARGS.
Used in advises."
  (if (and polymode-mode pm/polymode
           (not pm--killed)
           (buffer-live-p (buffer-base-buffer)))
      (let ((poly-lock-allow-fontification nil)
            (cur-buf (current-buffer))
            (base (buffer-base-buffer))
            (first-arg (car-safe args)))
        (with-current-buffer base
          (if (or (eq first-arg cur-buf)
                  (equal first-arg (buffer-name cur-buf)))
              (apply orig-fun base (cdr args))
            (apply orig-fun args))))
    (apply orig-fun args)))

(defun pm-around-advice (fun advice)
  "Apply around ADVICE to FUN.
If `advice-add` is available apply advice to FUN. If FUN is a
list, apply advice to each element of it."
  (when (and fun (fboundp 'advice-add))
    (cond ((listp fun)
           (dolist (el fun) (pm-around-advice el advice)))
          ((and (symbolp fun)
                (not (advice-member-p advice fun)))
           (advice-add fun :around advice)))))

(pm-around-advice #'kill-buffer #'polymode-with-current-base-buffer)
(pm-around-advice #'find-alternate-file #'polymode-with-current-base-buffer)
;; (advice-remove #'kill-buffer #'pm-with-current-base-buffer)
;; (advice-remove #'find-alternate-file #'pm-with-current-base-buffer)


;;; SYNTAX

;; fixme: this doesn't help with "din't move syntax-propertize--done" errors
(defun polymode-set-syntax-propertize-end (beg end)
  ;; syntax-propertize sets 'syntax-propertize--done to end in the original
  ;; buffer just before calling syntax-propertize-function; we do it in all
  ;; buffers in syntax-propertize-extend-region-functions because they are
  ;; called with syntax-propertize--done still unbound
  (dolist (b (oref pm/polymode -buffers))
    (with-current-buffer b
      ;; setq doesn't have effect because the var is let bound; set seems to work
      (set 'syntax-propertize--done end)))
  (cons beg end))

(defun pm--call-syntax-propertize-original (start end)
  (condition-case err
      (funcall pm--syntax-propertize-function-original start end)
    (error
     (message "ERROR: (%s %d %d) -> %s"
              (if (symbolp pm--syntax-propertize-function-original)
                  pm--syntax-propertize-function-original
                (format "polymode-syntax-propertize:%s" major-mode))
              start end
              ;; (backtrace)
              (error-message-string err)))))

;; called from syntax-propertize and thus at the beginning of syntax-ppss
(defun polymode-syntax-propertize (start end)
  ;; either this or polymode-set-syntax-propertize-end
  (dolist (b (oref pm/polymode -buffers))
    (with-current-buffer b
      ;; `setq' doesn't have an effect because the var is let bound; `set' works
      (set 'syntax-propertize--done end)))
  (unless pm-initialization-in-progress
    (save-restriction
      (widen)
      (save-excursion
        (when (or pm-verbose pm-syntax-verbose)
          (message "(polymode-syntax-propertize %d %d) [%s]" start end (current-buffer)))
        (let ((protect-host (with-current-buffer (pm-base-buffer)
                              (eieio-oref pm/chunkmode 'protect-syntax))))
          ;; 1. host if no protection
          (unless protect-host
            (with-current-buffer (pm-base-buffer)
              (when pm--syntax-propertize-function-original
                (when (or pm-verbose pm-syntax-verbose)
                  (message "(polymode-syntax-propertize %d %d) /unprotected host/ [%s]"
                           start end (current-buffer)))
                (pm--call-syntax-propertize-original start end))))
          ;; 2. all others
          (pm-map-over-spans
           (lambda ()
             (when (and pm--syntax-propertize-function-original
                        (or (pm-true-span-type *span*)
                            protect-host))
               (let ((pos0 (max (nth 1 *span*) start))
                     (pos1 (min (nth 2 *span*) end)))
                 (if (eieio-oref (nth 3 *span*) 'protect-syntax)
                     (pm-with-narrowed-to-span *span*
                       (pm--call-syntax-propertize-original pos0 pos1))
                   (pm--call-syntax-propertize-original pos0 pos1)))))
           start end))))))

(defun polymode-restrict-syntax-propertize-extension (orig-fun beg end)
  ;; (funcall orig-fun beg end)
  (if (and polymode-mode pm/polymode)
      (let ((span (pm-innermost-span beg)))
        (if (eieio-oref (nth 3 span) 'protect-syntax)
            (let ((range (pm-span-to-range span)))
              (if (and (eq beg (car range))
                       (eq end (cdr range)))
                  ;; in the most common case when span == beg-end, simply return
                  range
                (when (or pm-verbose pm-syntax-verbose)
                  (message "(polymode-restrict-syntax-propertize-extension -fn- %s %s) %s"
                           beg end (pm-format-span span)))
                (let ((be (funcall orig-fun beg end)))
                  (and be
                       (cons (max (car be) (car range))
                             (min (cdr be) (cdr range)))))))
          (when (or pm-verbose pm-syntax-verbose)
            (message "(syntax-propertize-extend-region %s %s) /unprotected/ %s"
                     beg end (pm-format-span span)))
          (funcall orig-fun beg end)))
    (funcall orig-fun beg end)))

(defvar syntax-ppss-wide)
(defvar syntax-ppss-last)
(defun pm--reset-ppss-last (span)
  "Reset `syntax-ppss-last' cache if it was recorded before SPAN's start."
  (let* ((sbeg (nth 1 span))
         (new-ppss (list sbeg 0 nil sbeg nil nil nil 0 nil nil nil nil)))
    ;; (when pm-syntax-verbose
    ;;   (message "reasserting PPSS %s" (pm-format-span span)))
    (if pm--emacs>26
        ;; in emacs 26 there are two caches syntax-ppss-wide and
        ;; syntax-ppss-narrow. The latter is reset automatically each time a
        ;; different narrowing is in place so we don't deal with it for now.
        (let ((cache (cdr syntax-ppss-wide)))
          (while (and cache (>= (caar cache) sbeg))
            (setq cache (cdr cache)))
          (setq cache (cons new-ppss cache))
          (setq syntax-ppss-wide (cons new-ppss cache)))
      (setq syntax-ppss-last new-ppss))))


;;; INTERNAL UTILITIES

(defvar polymode-display-output-file t
  "When non-nil automatically display output file in Emacs.
This is temporary variable, it might be changed or removed in the
near future.")

(defun pm--display-file (ofile)
  (when ofile
    ;; errors might occur (most notably with open-with package errors are intentional)
    ;; We need to catch those if we want to display multiple files like with Rmarkdown
    (condition-case err
        (let ((buff (get-file-buffer ofile)))
          ;; silently kill and re-open
          (when buff
            (with-current-buffer buff
              (revert-buffer t t)))
          (when polymode-display-output-file
            (if (string-match-p "html\\|htm$" ofile)
                (browse-url ofile)
              (display-buffer (find-file-noselect ofile 'nowarn)))))
      (error (message "Error while displaying '%s': %s"
                      (file-name-nondirectory ofile)
                      (error-message-string err))))))

(defun pm--symbol-name (str-or-symbol)
  (if (symbolp str-or-symbol)
      (symbol-name str-or-symbol)
    str-or-symbol))

(defun pm--get-mode-symbol-from-name (name)
  "Guess and return mode function from NAME or nil if not found."
  (if (and (symbolp name)
           (fboundp name))
      name
    (let* ((str (pm--symbol-name
                 (or (cdr (assq (intern (pm--symbol-name name))
                                polymode-mode-name-override-alist))
                     name)))
           (mname (if (string-match-p "-mode$" str)
                      str
                    (concat str "-mode"))))
      (or (pm--get-existent-mode (intern mname) t)
          (pm--get-existent-mode (intern (downcase mname)) t)))))

(defun pm--get-existent-mode (mode &optional no-fallback)
  "Check if MODE symbol is defined and is a valid function.
If so, return it, otherwise return `poly-fallback-mode' and issue
a warning. If NO-FALLBACK is non-nil, return nil otherwise return
`poly-fallback-mode'."
  (cond ((fboundp mode) mode)
        (no-fallback nil)
        (t 'poly-fallback-mode)))

(defun pm--oref-with-parents (object slot)
  "Merge slots SLOT from the OBJECT and all its parent instances."
  (let (VALS)
    (while object
      (setq VALS (append (and (slot-boundp object slot) ; don't cascade
                              (eieio-oref object slot))
                         VALS)
            object (and (slot-boundp object :parent-instance)
                        (eieio-oref object 'parent-instance))))
    VALS))

(defun pm--abrev-names (list abrev-regexp)
  "Abbreviate names in LIST by erasing ABREV-REGEXP matches.
Elements of LIST can be either strings or symbols."
  (mapcar (lambda (nm)
            (let ((str-nm (if (symbolp nm)
                              (symbol-name nm)
                            nm)))
              (cons (replace-regexp-in-string abrev-regexp "" str-nm)
                    str-nm)))
          list))

(defun pm--prop-put (key val &optional object)
  (oset (or object pm/polymode) -props
        (plist-put (oref (or object pm/polymode) -props) key val)))

(defun pm--prop-get (key &optional object)
  (plist-get (oref (or object pm/polymode) -props) key))

(defun pm--comment-region (beg end)
  ;; mark as syntactic comment
  (when (> end 1)
    (with-silent-modifications
      (let ((beg (or beg (region-beginning)))
            (end (or end (region-end))))
        (let ((ch-beg (char-after beg))
              (ch-end (char-before end)))
          (add-text-properties beg (1+ beg)
                               (list 'syntax-table (cons 11 ch-beg)
                                     'rear-nonsticky t
                                     'polymode-comment 'start))
          (add-text-properties (1- end) end
                               (list 'syntax-table (cons 12 ch-end)
                                     'rear-nonsticky t
                                     'polymode-comment 'end)))))))

(defun pm--uncomment-region (beg end)
  ;; Remove all syntax-table properties.
  ;; fixme: this beggs for problems
  (when (> end 1)
    (with-silent-modifications
      (let ((props '(syntax-table nil rear-nonsticky nil polymode-comment nil)))
        (remove-text-properties (max beg (point-min)) (min end (point-max)) props)
        ;; (remove-text-properties beg (1+ beg) props)
        ;; (remove-text-properties end (1- end) props)
        ))))

(defun pm--synchronize-points (&rest _ignore)
  "Synchronize points in all buffers.
IGNORE is there to allow this function in advises."
  (when polymode-mode
    (let ((pos (point))
          (cbuff (current-buffer)))
      (dolist (buff (eieio-oref pm/polymode '-buffers))
        (when (and (not (eq buff cbuff))
                   (buffer-live-p buff))
          (with-current-buffer buff
            (goto-char pos)))))))

(defun pm--completing-read (prompt collection &optional predicate require-match
                                   initial-input hist def inherit-input-method)
  ;; Wrapper for `completing-read'.
  ;; Take care when collection is an alist of (name . meta-info). If
  ;; so, asks for names, but returns meta-info for that name. Enforce
  ;; require-match = t. Also takes care of adding the most relevant
  ;; DEF from history.
  (if (and (listp collection)
           (listp (car collection)))
      (let* ((candidates (mapcar #'car collection))
             (thist (and hist
                         (delq nil (mapcar (lambda (x) (car (member x candidates)))
                                           (symbol-value hist)))))
             (def (or def (car thist))))
        (assoc (completing-read prompt candidates predicate t initial-input hist def inherit-input-method)
               collection))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))


;;; WEAVING and EXPORTING
;; fixme: move all these into separate polymode-process.el?
(defvar polymode-exporter-output-file-format)
(defvar polymode-weaver-output-file-format)
(declare-function pm-export "polymode-export")
(declare-function pm-weave "polymode-weave")
(declare-function comint-exec "comint")
(declare-function comint-mode "comint")

(defun pm--wrap-callback (processor slot _ifile)
  ;; replace processor :sentinel or :callback temporally in order to export-spec as a
  ;; followup step or display the result
  (let ((sentinel1 (eieio-oref processor slot))
        (cur-dir default-directory)
        (exporter (symbol-value (eieio-oref pm/polymode 'exporter)))
        (obuffer (current-buffer)))
    (if pm--export-spec
        (let ((espec pm--export-spec))
          (lambda (&rest args)
            (with-current-buffer obuffer
              (let ((wfile (apply sentinel1 args))
                    (pm--export-spec nil)
                    (pm--input-not-real t))
                ;; If no wfile, probably errors occurred. So we stop.
                (when wfile
                  (when (listp wfile)
                    ;; In an unlikely situation weaver can generate multiple
                    ;; files. Pick the first one.
                    (setq wfile (car wfile)))
                  (pm-export exporter (car espec) (cdr espec) wfile))))))
      (lambda (&rest args)
        (with-current-buffer obuffer
          (let ((ofile (apply sentinel1 args)))
            (when ofile
              (let ((ofiles (if (listp ofile) ofile (list ofile))))
                (dolist (f ofiles)
                  (pm--display-file (expand-file-name f cur-dir)))))))))))

(defun pm--file-mod-time (file)
  (and (stringp file)
       (file-exists-p file)
       (nth 5 (file-attributes file))))

(defvar-local pm--process-buffer nil)
;; Simplified version of TeX-run-TeX. Run shell COMMAND interactively in BUFFER.
;; Run COMMAND in a buffer (in comint-shell-mode) in order to be able to accept
;; user interaction.
(defun pm--run-shell-command (command sentinel buff-name message)
  (require 'comint)
  (let* ((buffer (get-buffer-create buff-name))
         (process nil)
         ;; weave/export buffers are re-usable; need to transfer some vars
         (dd default-directory)
         ;; (command (shell-quote-argument command))
         )
    (with-current-buffer buffer
      (setq-local default-directory dd)
      (read-only-mode -1)
      (erase-buffer)
      (insert message)
      (comint-exec buffer buff-name shell-file-name nil
                   (list shell-command-switch command))
      (setq process (get-buffer-process buffer))
      (comint-mode)
      (set-process-sentinel process sentinel)
      (setq pm--process-buffer t)
      (set-marker (process-mark process) (point-max))
      ;; for communication with sentinel
      (process-put process :output-file pm--output-file)
      (process-put process :output-file-mod-time (pm--file-mod-time pm--output-file))
      (process-put process :input-file pm--input-file)
      (when polymode-display-process-buffers
        (display-buffer buffer `(nil . ((inhibit-same-window . ,pop-up-windows)))))
      nil)))

(defun pm--make-shell-command-sentinel (action)
  (lambda (process _name)
    "Sentinel built with `pm--make-shell-command-sentinel'."
    (let ((buff (process-buffer process))
          (status (process-exit-status process)))
      (if (> status 0)
          (progn
            (message "Errors during %s; process exit status %d" action status)
            (ding) (sit-for 1)
            nil)
        (with-current-buffer buff
          (let ((ofile (process-get process :output-file)))
            (cond
             ;; 1. output-file guesser
             ((functionp ofile) (funcall ofile))
             ;; 2. string
             (ofile
              (let ((otime (process-get process :output-file-mod-time))
                    (ntime (pm--file-mod-time ofile)))
                (if (or (null ntime)
                        (and otime
                             (not (time-less-p otime ntime))))
                    ;; mod time didn't change
                    ;; tothink: shall we still return ofile for display?
                    (progn
                      (display-buffer (current-buffer))
                      (message "Output file unchanged. Either input unchanged or errors during %s." action)
                      (ding) (sit-for 1)
                      ofile)
                  ;; else, all is good, we return the file name
                  ;; (display-buffer (current-buffer))
                  (message "Done with %s" action)
                  ofile)))
             ;; 3. output file is not known; display process buffer
             (t (display-buffer (current-buffer)) nil))))))))

(fset 'pm-default-export-sentinel (pm--make-shell-command-sentinel "export"))
(fset 'pm-default-shell-weave-sentinel (pm--make-shell-command-sentinel "weaving"))

(defun pm--make-selector (specs elements)
  (cond ((listp elements)
         (let ((spec-alist (cl-mapcar #'cons specs elements)))
           (lambda (selsym &rest _ignore)
             (cdr (assoc selsym spec-alist)))))
        ((functionp elements) elements)
        (t (error "Elements argument must be either a list or a function"))))

(defun pm--selector (processor type id)
  (let ((spec (or (assoc id (eieio-oref processor type))
                  (error "%s spec '%s' cannot be found in '%s'"
                         (symbol-name type) id (eieio-object-name processor))))
        (names (cond
                ;; exporter slots
                ((eq type :from) '(regexp doc command))
                ((eq type :to) '(ext doc t-spec))
                ;; weaver slot
                ((eq type :from-to) '(regexp ext doc command))
                (t (error "Invalid type '%s'" type)))))
    (pm--make-selector names (cdr spec))))

(defun pm--selector-match (selector &optional file)
  (or (funcall selector 'match file)
      (string-match-p (funcall selector 'regexp)
                      (or file buffer-file-name))))

(defun pm--selectors (processor type)
  (let ((ids (mapcar #'car (eieio-oref processor type))))
    (mapcar (lambda (id) (cons id (pm--selector processor type id))) ids)))

(defun pm--output-command.file (output-file-format sfrom &optional sto quote)
  ;; !!Must be run in input buffer!!
  (cl-flet ((squote (arg) (or (and (stringp arg)
                                   (if quote (shell-quote-argument arg) arg))
                              "")))
    (let* ((base-ofile (or (funcall (or sto sfrom) 'output-file)
                           (let ((ext (funcall (or sto sfrom) 'ext)))
                             (when ext
                               (concat (format output-file-format
                                               (file-name-base buffer-file-name))
                                       "." ext)))))
           (ofile (and (stringp base-ofile)
                       (expand-file-name base-ofile)))
           (oname (and (stringp base-ofile)
                       (file-name-base base-ofile)))
           (t-spec (and sto (funcall sto 't-spec)))
           (command-w-formats (or (and sto (funcall sto 'command))
                                  (and (listp t-spec) (car t-spec))
                                  (funcall sfrom 'command)))
           (command (format-spec command-w-formats
                                 (list (cons ?i (squote (file-name-nondirectory buffer-file-name)))
                                       (cons ?I (squote buffer-file-name))
                                       (cons ?o (squote base-ofile))
                                       (cons ?O (squote ofile))
                                       (cons ?b (squote oname))
                                       (cons ?t (squote t-spec))))))
      (cons command (or ofile base-ofile)))))

(defun pm--process-internal (processor from to ifile &optional callback quote)
  (let ((is-exporter (object-of-class-p processor 'pm-exporter)))
    (if is-exporter
        (unless (and from to)
          (error "For exporter both FROM and TO must be supplied (from: %s, to: %s)" from to))
      (unless from
        ;; it represents :from-to slot
        (error "For weaver FROM must be supplied (from: %s)" from)))
    (let* ((sfrom (if is-exporter
                      (pm--selector processor :from from)
                    (pm--selector processor :from-to from)))
           (sto (and is-exporter (pm--selector processor :to to)))
           (ifile (or ifile buffer-file-name))
           ;; fixme: nowarn is only right for inputs from weavers, you need to
           ;; save otherwise
           (ibuffer (if pm--input-not-real
                        ;; for exporter input we silently re-fetch the file
                        ;; even if it was modified
                        (find-file-noselect ifile t)
                      ;; if real user file, get it or fetch it
                      (or (get-file-buffer ifile)
                          (find-file-noselect ifile))))
           (output-format (if is-exporter
                              polymode-exporter-output-file-format
                            polymode-weaver-output-file-format)))
      (with-current-buffer ibuffer
        (save-buffer)
        (let ((comm.ofile (pm--output-command.file output-format sfrom sto quote)))
          (message "%s '%s' with '%s' ..." (if is-exporter "Exporting" "Weaving")
                   (file-name-nondirectory ifile) (eieio-object-name processor))
          (let* ((pm--output-file (cdr comm.ofile))
                 (pm--input-file ifile)
                 ;; skip weaving step if possible
                 ;; :fixme this should not happen after weaver/exporter change
                 ;; or after errors in previous exporter
                 (omt (and polymode-skip-processing-when-unmodified
                           (stringp pm--output-file)
                           (pm--file-mod-time pm--output-file)))
                 (imt (and omt (pm--file-mod-time pm--input-file)))
                 (ofile (or (and imt (time-less-p imt omt) pm--output-file)
                            (let ((fun (with-no-warnings
                                         (eieio-oref processor 'function)))
                                  (args (delq nil (list callback from to))))
                              (apply fun (car comm.ofile) args)))))
            ;; ofile is non-nil in two cases:
            ;;  -- synchronous back-ends (very uncommon)
            ;;  -- when output is transitional (not real) and mod time of input < output
            (when ofile
              (if pm--export-spec
                  ;; same logic as in pm--wrap-callback
                  (let ((pm--input-not-real t)
                        (espec pm--export-spec)
                        (pm--export-spec nil))
                    (when (listp ofile)
                      (setq ofile (car ofile)))
                    (pm-export (symbol-value (eieio-oref pm/polymode 'exporter))
                               (car espec) (cdr espec)
                               ofile))
                (pm--display-file ofile)))))))))

(provide 'polymode-core)
;;; polymode-core.el ends here
