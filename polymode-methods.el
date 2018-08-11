;;; polymode-methods.el --- Methods for polymode classes -*- lexical-binding: t -*-
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
;;; Code:

(require 'polymode-core)
(require 'poly-lock)


;;; INITIALIZATION

(defgeneric pm-initialize (object)
  "Initialize current buffer with OBJECT.")

(defmethod pm-initialize ((config pm-polymode))
  "Initialization of host buffers.
Ran directly by the polymode modes."
  (let ((chunkmode (clone (symbol-value (oref config :hostmode)))))
    (let ((pm-initialization-in-progress t)
          ;; Set if nil! This allows unspecified host chunkmodes to be used in
          ;; minor modes.
          (host-mode (or (oref chunkmode :mode)
                         (oset chunkmode :mode major-mode))))
      ;; host-mode hooks are run here, but polymode is not initialized
      (pm--mode-setup host-mode)
      ;; FIXME: inconsistency?
      ;; Not calling config's :minor-mode (polymode function). But polymode
      ;; function calls pm-initialize, so it's probably ok.
      (oset chunkmode -buffer (current-buffer))
      (oset config -hostmode chunkmode)
      (setq pm/polymode config
            pm/chunkmode chunkmode
            pm/type 'host)
      (pm--common-setup)
      ;; Initialize innermodes
      (oset config -innermodes
            (mapcar (lambda (sub-name)
                      (clone (symbol-value sub-name)))
                    (oref config :innermodes)))
      ;; FIXME: must go into polymode-compat.el
      (add-hook 'flyspell-incorrect-hook
                'pm--flyspel-dont-highlight-in-chunkmodes nil t))
    (pm--run-init-hooks config 'polymode-init-host-hook)
    (pm--run-init-hooks chunkmode)))

(defmethod pm-initialize ((chunkmode pm-chunkmode) &optional type mode)
  "Initialization of chunk (indirect) buffers."
  ;; run in chunkmode indirect buffer
  (setq mode (or mode (pm--get-chunkmode-mode chunkmode type)))
  (let ((pm-initialization-in-progress t)
        (new-name  (generate-new-buffer-name
                    (format "%s[%s]" (buffer-name (pm-base-buffer))
                            (replace-regexp-in-string "-mode" "" (symbol-name mode))))))
    (rename-buffer new-name)
    (pm--mode-setup (pm--get-existent-mode mode))
    (pm--move-vars '(pm/polymode buffer-file-coding-system) (pm-base-buffer))
    (setq pm/chunkmode chunkmode
          pm/type type)
    ;; Call polymode mode for the sake of the keymap. Same minor mode which runs
    ;; in the host buffer but without all the heavy initialization.
    (funcall (oref pm/polymode :minor-mode))
    ;; FIXME: should not be here?
    (vc-find-file-hook)
    (pm--common-setup)
    (pm--run-init-hooks chunkmode 'polymode-init-inner-hook)))

(defun pm--mode-setup (mode &optional buffer)
  ;; General major-mode install. Should work for both indirect and base buffers.
  ;; PM objects are not yet initialized (pm/polymode, pm/chunkmode, pm/type)
  (with-current-buffer (or buffer (current-buffer))
    ;; don't re-install if already there; polymodes can be used as minor modes.
    (unless (eq major-mode mode)
      (let ((polymode-mode t)           ;major-modes might check this
            ;; (font-lock-fontified t)
            ;; Modes often call font-lock functions directly. We prevent that.
            (font-lock-function 'ignore)
            (font-lock-flush-function 'ignore)
            (font-lock-fontify-buffer-function 'ignore)
            ;; Mode functions can do arbitrary things. We inhibt all PM hooks
            ;; because PM objects have not been setup yet.
            (pm-allow-after-change-hook nil)
            (poly-lock-allow-fontification nil))
        (condition-case-unless-debug err
            (funcall mode)
          (error (message "Polymode error (pm--mode-setup '%s): %s" mode (error-message-string err))))))
    (setq polymode-mode t)
    (current-buffer)))

(defvar-local pm--indent-line-function-original nil)
(defvar-local pm--syntax-propertize-function-original nil)
(defun pm--common-setup (&optional buffer)
  "Common setup for all polymode buffers.
Runs after major mode and core polymode structures have been
initialized. Return the buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when pm-verbose
      (message "common-setup for %s: [%s]" major-mode (current-buffer)))
    (object-add-to-list pm/polymode '-buffers (current-buffer))

    ;; INDENTATION
    (when (and indent-line-function     ; not that it should ever be nil...
               (oref pm/chunkmode :protect-indent-line))
      (setq-local pm--indent-line-function-original indent-line-function)
      (setq-local indent-line-function #'pm-indent-line-dispatcher))

    ;; SYNTAX
    ;; ideally this should be called in some hook to avoid minor-modes messing it up
    (when (and syntax-propertize-function
               (not (eq syntax-propertize-function #'polymode-syntax-propertize)))
      (setq-local pm--syntax-propertize-function-original syntax-propertize-function)
      (setq-local syntax-propertize-function #'polymode-syntax-propertize))
    ;; [OBSOLETE as of 25.1] We know that syntax starts at span start.
    (pm-around-advice syntax-begin-function 'pm-override-output-position)
    ;; Make sure that none of the `syntax-propertize-extend-region-functions'
    ;; extends the region beyond current span. In practice the only negative
    ;; effect of such extra-extension is slight performance hit. Not sure here,
    ;; but there might be situations when extending beyond current span does
    ;; make sense. Remains to be seen.
    (pm-around-advice syntax-propertize-extend-region-functions
                      #'polymode-restrict-syntax-propertize-extension)
    ;; flush ppss in all buffers and hook checks
    (add-hook 'before-change-functions 'polymode-before-change-setup t t)
    (setq-local syntax-ppss-narrow (cons nil nil))
    (setq-local syntax-ppss-wide (cons nil nil))

    ;; HOOKS
    (add-hook 'kill-buffer-hook #'polymode-after-kill-fixes t t)
    (add-hook 'post-command-hook #'polymode-post-command-select-buffer nil t)

    ;; FONT LOCK (see poly-lock.el)
    (setq-local font-lock-function 'poly-lock-mode)
    ;; Font lock is initialized `after-change-major-mode-hook' by means of
    ;; `run-mode-hooks' and poly-lock won't get installed if polymode is
    ;; installed as minor mode or interactively.
    ;; Use font-lock-mode instead of poly-lock-mode because modes which don't
    ;; want font-lock can simply set `font-lock-mode' to nil.
    (font-lock-mode t)
    (font-lock-flush)

    (current-buffer)))

(defun pm--run-init-hooks (object &optional emacs-hook)
  (unless pm-initialization-in-progress
    (when emacs-hook
      (run-hooks emacs-hook))
    (pm--run-hooks object :init-functions)))

(defun pm--run-hooks (object slot &rest args)
  "Run hooks from SLOT of OBJECT and its parent instances.
Parents' hooks are run first."
  (let ((inst object)
        funs)
    ;; run hooks, parents first
    (while inst
      (setq funs (append (and (slot-boundp inst slot) ; don't cascade
                              (eieio-oref inst slot))
                         funs)
            inst (and (slot-boundp inst :parent-instance)
                      (oref inst :parent-instance))))
    (if args
        (apply 'run-hook-with-args 'funs args)
      (run-hooks 'funs))))


;;; BUFFER CREATION

(defgeneric pm-get-buffer-create (chunkmode &optional type)
  "Get the indirect buffer associated with SUBMODE and SPAN-TYPE.
Create and initialize the buffer if does not exist yet.")

(defmethod pm-get-buffer-create ((chunkmode pm-chunkmode) &optional type)
  (let ((buff (oref chunkmode -buffer)))
    (or (and (buffer-live-p buff) buff)
        (oset chunkmode -buffer
              (pm--get-chunkmode-buffer-create chunkmode type)))))

(defmethod pm-get-buffer-create ((chunkmode pm-inner-chunkmode) &optional type)
  (let ((buff (cond ((eq 'body type) (oref chunkmode -buffer))
                    ((eq 'head type) (oref chunkmode -head-buffer))
                    ((eq 'tail type) (oref chunkmode -tail-buffer))
                    (t (error "Don't know how to select buffer of type '%s' for chunkmode '%s' of class '%s'"
                              type (eieio-object-name chunkmode) (class-of chunkmode))))))
    (if (buffer-live-p buff)
        buff
      (pm--set-chunkmode-buffer chunkmode type
                                (pm--get-chunkmode-buffer-create chunkmode type)))))

(defun pm--get-chunkmode-buffer-create (chunkmode type)
  (let ((mode (pm--get-existent-mode
               (pm--get-chunkmode-mode chunkmode type))))
    (or
     ;; 1. look through existent buffer list
     (cl-loop for bf in (oref pm/polymode -buffers)
              when (and (buffer-live-p bf)
                        (eq mode (buffer-local-value 'major-mode bf)))
              return bf)
     ;; 2. create new
     (with-current-buffer (pm-base-buffer)
       (let* ((new-name (generate-new-buffer-name (buffer-name)))
              (new-buffer (make-indirect-buffer (current-buffer) new-name)))
         (with-current-buffer new-buffer
           (pm-initialize chunkmode type mode))
         new-buffer)))))

(defun pm--get-chunkmode-mode (obj type)
  (with-slots (mode head-mode tail-mode) obj
    (cond ((or (eq type 'body)
               (and (eq type 'head)
                    (eq head-mode 'body))
               (and (eq type 'tail)
                    (or (eq tail-mode 'body)
                        (and (or (null tail-mode)
                                 (eq tail-mode 'head))
                             (eq head-mode 'body)))))
           (oref obj :mode))
          ((or (and (eq type 'head)
                    (eq head-mode 'host))
               (and (eq type 'tail)
                    (or (eq tail-mode 'host)
                        (and (or (null tail-mode)
                                 (eq tail-mode 'head))
                             (eq head-mode 'host)))))
           (oref (oref pm/polymode -hostmode) :mode))
          ((eq type 'head)
           (oref obj :head-mode))
          ((eq type 'tail)
           (if (or (null tail-mode)
                   (eq tail-mode 'head))
               (oref obj :head-mode)
             (oref obj :tail-mode)))
          (t (error "type must be one of 'head 'tail 'body")))))

(defun pm--set-chunkmode-buffer (obj type buff)
  "Assign BUFF to OBJ's slot(s) corresponding to TYPE."
  (with-slots (-buffer head-mode -head-buffer tail-mode -tail-buffer) obj
    (pcase (list type head-mode tail-mode)
      (`(body body ,(or `nil `body))
       (setq -buffer buff
             -head-buffer buff
             -tail-buffer buff))
      (`(body ,_ body)
       (setq -buffer buff
             -tail-buffer buff))
      (`(body ,_ ,_ )
       (setq -buffer buff))
      (`(head ,_ ,(or `nil `head))
       (setq -head-buffer buff
             -tail-buffer buff))
      (`(head ,_ ,_)
       (setq -head-buffer buff))
      (`(tail ,_ ,(or `nil `head))
       (setq -tail-buffer buff
             -head-buffer buff))
      (`(tail ,_ ,_)
       (setq -tail-buffer buff))
      (_ (error "type must be one of 'body, 'head or 'tail")))))


;;; SPAN MANIPULATION

(defgeneric pm-get-span (chunkmode &optional pos)
  "Ask the CHUNKMODE for the span at point.
Return a list of three elements (TYPE BEG END OBJECT) where TYPE
is a symbol representing the type of the span surrounding
POS (head, tail, body). BEG and END are the coordinates of the
span. OBJECT is a suitable object which is 'responsible' for this
span. This is an object that could be dispatched upon with
`pm-select-buffer'. Should return nil if there is no SUBMODE
specific span around POS. Not to be used in programs directly;
use `pm-get-innermost-span'.")

(defmethod pm-get-span ((chunkmode pm-inner-chunkmode) &optional pos)
  "Return a list of the form (TYPE POS-START POS-END SELF).
TYPE can be 'body, 'head or 'tail. SELF is just a chunkmode object
in this case."
  (with-slots (head-matcher tail-matcher head-mode tail-mode) chunkmode
    (let ((span (pm--span-at-point head-matcher tail-matcher pos)))
      (when span
        (let ((type (car span)))
          (if (or (and (eq type 'head) (eq head-mode 'host))
                  (and (eq type 'tail) (or (eq tail-mode 'host)
                                           (and (null tail-mode)
                                                (eq head-mode 'host)))))
              (list nil (nth 1 span) (nth 2 span) (oref pm/polymode -hostmode))
            (append span (list chunkmode))))))))

(defmethod pm-get-span ((chunkmode pm-inner-auto-chunkmode) &optional pos)
  (let ((span (call-next-method)))
    (if (null (car span))
        span
      (setf (nth 3 span) (pm--get-auto-chunk span))
      span)))

(defun pm--get-auto-chunk (span)
  ;; fixme: cache somehow?
  (let ((type (car span))
        (proto (nth 3 span)))
    (save-excursion
      (goto-char (nth 1 span))
      (unless (eq type 'head)
        (let ((matcher (oref proto :head-matcher)))
          (cond ((functionp matcher)
                 (goto-char (car (funcall matcher -1))))
                ((consp matcher)
                 (when (> (cdr matcher) 0)
                   (goto-char (nth 2 span)))
                 (and (re-search-backward (car matcher) nil 'noerr)
                      (goto-char (match-beginning (cdr matcher)))))
                ((stringp matcher)
                 (re-search-backward matcher nil 'noerr))
                (t (error "invalid head matcher: %s" matcher)))))
      (let* ((str (let* ((matcher (oref proto :mode-matcher))
                         (matcher (if (stringp matcher)
                                      (cons matcher 0)
                                    matcher)))
                    (cond  ((consp matcher)
                            (re-search-forward (car matcher) (point-at-eol) t)
                            (match-string-no-properties (cdr matcher)))
                           ((functionp matcher)
                            (funcall matcher)))))
             (mode (and str (pm--get-mode-symbol-from-name str 'no-fallback))))
        (if mode
            ;; Inferred body MODE serves as ID; this not need be the case in the
            ;; future and a generic id getter might replace it. Currently
            ;; head/tail/body indirect buffers are shared across chunkmodes.
            ;; This currently works ok. A more general approach would be to
            ;; track head/tails/body with associated chunks. Then for example R
            ;; hbt-chunk and elisp hbt-chunk will not share head/tail buffers.
            ;; There could be even two R hbt-chunks providing different
            ;; functionality and thus not even sharing body buffer.
            (let ((name (concat (object-name-string proto) ":" (symbol-name mode))))
              (or
               ;; a. loop through installed inner modes
               (cl-loop for obj in (oref pm/polymode -auto-innermodes)
                        when (equal name (object-name-string obj))
                        return obj)
               ;; b. create new
               (let ((innermode (clone proto name :mode mode)))
                 (object-add-to-list pm/polymode '-auto-innermodes innermode)
                 innermode)))
          ;; else, use hostmode
          (oref pm/polymode -hostmode))))))

(defmacro pm-create-indented-block-matchers (name regex)
  "Defines 2 functions, each return a list of the start and end points of the
HEAD and TAIL portions of an indented block of interest, via some regex.
You can then use these functions in the defcustom pm-inner modes. E.g.

 (pm-create-indented-block-matchers 'slim-coffee' \"^[^ ]*\\(.*:? *coffee: *\\)$\")

would create the functions pm-slim-coffee-head-matcher and
pm-slim-coffee-tail-matcher.

In the example below,

The head matcher will match against 'coffee:', returning the positions of the
start and end of 'coffee:'
The tail matcher will return a list (n, n) of the final characters is the block.
g
    |<----- Uses this indentation to define the left edge of the 'block'
    |
    |<--->|  This region is higlighted by the :head-mode in the block-matchers
    |     |
    |     |<----- the head matcher uses this column as the end of the head
    |     |
----:-----:-------------- example file -----------------------------------------
1|  :     :
2|  coffee:
3|    myCoffeeCode()
4|    moreCode ->
5|      do things
6|              :
7|  This is no longer in the block
8|              :
----------------:---------------------------------------------------------------
            --->|<----- this region of 0 width is highlighted by the :tail-mode
                        the 'block' ends after this column on line 5


All the stuff after the -end- of the head and before the start of the tail is
sent to the new mode for syntax highlighting."
  (let* ((head-name (intern (format "pm-%s-head-matcher" name)))
         (tail-name (intern (format "pm-%s-tail-matcher" name))))
    `(progn
       (defun ,head-name (ahead)
         (when (re-search-forward ,regex nil t ahead)
           (cons (match-beginning 1) (match-end 1))))
       (defun ,tail-name (ahead)
         (save-excursion
           ;; (cons (point-max) (point-max)))))))
           (goto-char (car (,head-name 1)))
           (let* ((block-col (current-indentation))
                  (posn (catch 'break
                          (while (not (eobp))
                            (forward-line 1)
                            (when (and (<= (current-indentation) block-col)
                                       (not (progn
                                              (beginning-of-line)
                                              (looking-at "^[[:space:]]*$"))))
                              (throw 'break (point-at-bol))))
                          (throw 'break (point-max)))))
             (cons posn posn)))))))

(defun pm--default-matcher (reg subexpr ahead)
  (if (< ahead 0)
      (if (re-search-backward reg nil t)
          (cons (match-beginning subexpr) (match-end subexpr)))
    (if (re-search-forward reg nil t)
        (cons (match-beginning subexpr) (match-end subexpr)))))

;; fixme: there should be a simpler way... check the code and document
;; VS[10-08-2018]: should follow same logic as reg-reg version?
(defun pm--span-at-point-fun-fun (hd-matcher tl-matcher)
  (save-excursion
    (let ((pos (point))
          (posh (funcall hd-matcher -1)))
      (if (null posh)
          ;; special first chunk
          (let ((posh1 (progn (goto-char (point-min))
                              (funcall hd-matcher 1))))
            (if (and posh1
                     (<= (car posh1) pos)
                     (< pos (cdr posh1)))
                (list 'head (car posh1) (cdr posh1))
              (list nil (point-min) (or (car posh1)
                                        (point-max)))))
        (let ((post (progn (goto-char (car posh))
                           (or (funcall tl-matcher 1)
                               (cons (point-max) (point-max))))))
          (if (and (<= (cdr posh) pos)
                   (< pos (car post)))
              (list 'body (cdr posh) (car post))
            (if (and (<= (car post) pos)
                     (< pos (cdr post)))
                (list 'tail (car post) (cdr post))
              (if (< pos (cdr post))
                  ;; might be in the head
                  (progn
                    (goto-char (car post))
                    (let ((posh1 (funcall hd-matcher -1)))
                      (if (and (<= (car posh1) pos)
                               (< pos (cdr posh1)))
                          (list 'head (car posh1) (cdr posh1))
                        (list nil (cdr posh) (car posh1))))) ;; posh is point min, fixme: not true anymore?
                (goto-char (cdr post))
                (let ((posh1 (or (funcall hd-matcher 1)
                                 (cons (point-max) (point-max)))))
                  (if (and posh
                           (<= (car posh1) pos )
                           (< pos (cdr posh1)))
                      (list 'head (car posh1) (cdr posh1))
                    (list nil (cdr post) (car posh1))))))))))))

(defun pm--span-at-point-reg-reg (head-matcher tail-matcher)
  ;; - head-matcher and tail-matcher are conses of the form (REG . SUBEXPR-NUM).
  ;; - Guaranteed to produce non-0 length spans or nil if no span has been found.
  (save-excursion
    (let* ((pos (point))
           (reg-head (car head-matcher))
           (sub-head (cdr head-matcher))
           (reg-tail (car tail-matcher))
           (sub-tail (cdr tail-matcher))
           (at-max (eq pos (point-max)))
           (head1-beg (and (re-search-backward reg-head nil t)
                           (match-beginning sub-head)))
           (head1-end (and head1-beg (match-end sub-head))))
      (if head1-end
          (if (and at-max (eq head1-end pos))
              ;;           |
              ;; host)[head)           ; can occur with sub-head == 0 only
              (list 'head head1-beg head1-end)
            ;;            -----------------------
            ;; host)[head)[body)[tail)[host)[head)
            (pm--find-tail-from-head pos head1-end reg-head sub-head reg-tail sub-tail))
        ;; ----------
        ;; host)[head)[body)[tail)[host
        (let ((head2-beg (and (goto-char (point-min))
                              ;; fixme: optimization opportunity: if we know
                              ;; that head-tail cannot span multilines there is
                              ;; no need to search till the end of buffer, just
                              ;; till (pos-at-eol)
                              (re-search-forward reg-head nil t)
                              (match-beginning sub-head))))
          (if head2-beg
              (if (< pos head2-beg)
                  ;; ----
                  ;; host)[head)[body)[tail)[host
                  (list nil (point-min) head2-beg)
                (if (< pos (match-end sub-head))
                    ;;      -----
                    ;; host)[head)[body)[tail)[host
                    (list 'head head2-beg (match-end sub-head))
                  ;;            -----------------
                  ;; host)[head)[body)[tail)[host       ; can happen only when sub-head > 0
                  (pm--find-tail-from-head pos (match-end sub-head) reg-head sub-head reg-tail sub-tail)))
            ;; no span found
            nil))))))

(defun pm--find-tail-from-head (pos head-end reg-head sub-head reg-tail sub-tail)
  (let* ((tail1-beg (and (goto-char head-end)
                         (re-search-forward reg-tail nil t)
                         (match-beginning sub-tail)))
         (tail1-end (and tail1-beg (match-end sub-tail))))
    (if tail1-end
        (if (< pos tail1-end)
            (if (< pos tail1-beg)
                ;;            -----
                ;; host)[head)[body)[tail)[host)[head)
                (list 'body head-end tail1-beg)
              ;;                  -----
              ;; host)[head)[body)[tail)[host)[head)
              (list 'tail tail1-beg tail1-end))
          (if (and (= pos (point-max)) (eq tail1-end pos))
              ;;                       |
              ;; host)[head)[body)[tail)           ; can happen when sub-tail == 0 only
              (list 'tail tail1-beg tail1-end)
            ;;                        -----------
            ;; host)[head)[body)[tail)[host)[head)
            (let* ((head2-beg (and (re-search-forward reg-head nil t)
                                   (match-beginning sub-head)))
                   (head2-end (and head2-beg (match-end sub-head))))
              (if head2-beg
                  (if (< pos head2-beg)
                      ;;                        -----
                      ;; host)[head)[body)[tail)[host)[head)
                      (list nil tail1-end head2-beg)
                    (if (or (< pos head2-end)
                            (and (= pos (point-max))
                                 (eq head2-end pos)))
                        ;;                              -----
                        ;; host)[head)[body)[tail)[host)[head)[body
                        (list 'head head2-beg head2-end)
                      ;;                                    ----
                      ;; host)[head)[body)[tail)[host)[head)[body
                      (pm--find-tail-from-head pos head2-end reg-head sub-head reg-tail sub-tail)))
                ;;                        -----
                ;; host)[head)[body)[tail)[host)
                (list nil tail1-end (point-max))))))
      ;;            -----
      ;; host)[head)[body)
      (list 'body head-end (point-max)))))

(defun pm--span-at-point (head-matcher tail-matcher &optional pos)
  "Basic span detector with head/tail.
Either of HEAD-MATCHER and TAIL-MATCHER can be a regexp or a
function. When a function the matcher must accept one argument
that can take either values 1 (forwards search) or -1 (backward
search). This function must return either nil (no match) or
a (cons BEG END) representing the span of the head or tail
respectively. See `pm--default-matcher' for an example.

Return (type span-start span-end) where type is one of the
follwoing symbols:
  nil   - pos is between point-min and head-matcher, or between tail-matcher and point-max
  body  - pos is between head-matcher and tail-matcher (exclusively)
  head  - head span
  tail  - tail span
"
  ;; ! start of the span is part of the span !
  (save-restriction
    (widen)
    (goto-char (or pos (point)))
    (let ((head-matcher (if (stringp head-matcher)
                            (cons head-matcher 0)
                          head-matcher))
          (tail-matcher (if (stringp tail-matcher)
                            (cons tail-matcher 0)
                          tail-matcher)))
      (cond ((and (or (consp head-matcher))
                  (or (consp tail-matcher)))
             (pm--span-at-point-reg-reg head-matcher tail-matcher))
            ((and (consp head-matcher)
                  (functionp tail-matcher))
             (pm--span-at-point-fun-fun
              (lambda (ahead) (pm--default-matcher (car head-matcher) (cdr head-matcher) ahead))
              tail-matcher))
            ((and (functionp head-matcher)
                  (consp tail-matcher))
             (pm--span-at-point-fun-fun
              head-matcher
              (lambda (ahead) (pm--default-matcher (car tail-matcher) (cdr tail-matcher) ahead))))
            ((and (functionp head-matcher)
                  (functionp tail-matcher))
             (pm--span-at-point-fun-fun head-matcher tail-matcher))
            (t (error "head and tail matchers must be either regexp strings, cons cells or functions"))))))


;;; INDENT

(defun pm--indent-line (span)
  (let (point)
    (save-current-buffer
      (pm-set-buffer span)
      (pm-with-narrowed-to-span span
        (funcall pm--indent-line-function-original)
        (setq point (point))))
    (goto-char point)))

(defun pm-indent-line-dispatcher ()
  "Dispatch methods indent methods on current span.
Value of `indent-line-function' in polymode buffers."
  (let ((span (pm-get-innermost-span))
        (inhibit-read-only t))
    (pm-indent-line (car (last span)) span)))

(defgeneric pm-indent-line (&optional chunkmode span)
  "Indent current line.
Protect and call original indentation function associated with
the chunkmode.")

(defmethod pm-indent-line ((chunkmode pm-chunkmode) &optional span)
  (pm--indent-line span))

(defmethod pm-indent-line ((chunkmode pm-inner-chunkmode) &optional span)
  "Indent line in inner chunkmodes.
When point is at the beginning of head or tail, use parent chunk
to indent."
  (let ((pos (point))
        (span (or span (pm-get-innermost-span)))
        delta)
    (unwind-protect
        (cond

         ;; 1. in head or tail (we assume head or tail fit in one line for now)
         ((or (eq 'head (car span))
              (eq 'tail (car span)))
          (goto-char (nth 1 span))
          (setq delta (- pos (point)))
          (when (not (bobp))
            (let ((prev-span (pm-get-innermost-span (1- pos))))
              (if (and (eq 'tail (car span))
                       (eq (point) (save-excursion (back-to-indentation) (point))))
                  ;; if tail is first on the line, indent as head
                  (indent-to (pm--head-indent prev-span))
                (pm--indent-line prev-span)))))

         ;; 2. body
         (t
          (back-to-indentation)
          (if (> (nth 1 span) (point))
              ;; first body line in the same line with header (re-indent at indentation)
              (pm-indent-line-dispatcher)
            (setq delta (- pos (point)))
            (pm--indent-line span)
            (let ((fl-indent (pm--first-line-indent span)))
              (if fl-indent
                  (when (bolp)
                    ;; Not first line. Indent only when original indent is at
                    ;; 0. Otherwise it's a continuation indentation and we assume
                    ;; the original function did it correctly with respect to
                    ;; previous lines.
                    (indent-to fl-indent))
                ;; First line. Indent with respect to header line.
                (indent-to
                 (+ (- (point) (point-at-bol)) ;; non-0 if code in header line
                    (pm--head-indent span) ;; indent with respect to header line
                    (oref chunkmode :indent-offset))))))))
      ;; keep point on same characters
      (when (and delta (> delta 0))
        (goto-char (+ (point) delta))))))

(defun pm--first-line-indent (&optional span)
  (save-excursion
    (let ((pos (point)))
      (goto-char (nth 1 (or span (pm-get-innermost-span))))
      (goto-char (point-at-eol))
      (skip-chars-forward " \t\n")
      (let ((indent (- (point) (point-at-bol))))
        (when (< (point-at-eol) pos)
          indent)))))

(defun pm--head-indent (&optional span)
  (save-excursion
    (goto-char (nth 1 (or span (pm-get-innermost-span))))
    (back-to-indentation)
    (- (point) (point-at-bol))))


;;; FACES
(defgeneric pm-get-adjust-face (chunkmode &optional type))

(defmethod pm-get-adjust-face ((chunkmode pm-chunkmode) &optional type)
  (oref chunkmode :adjust-face))

(defmethod pm-get-adjust-face ((chunkmode pm-inner-chunkmode) &optional type)
  (setq type (or type pm/type))
  (cond ((eq type 'head)
         (oref chunkmode :head-adjust-face))
        ((eq type 'tail)
         (if (eq 'head (oref pm/chunkmode :tail-adjust-face))
             (oref pm/chunkmode :head-adjust-face)
           (oref pm/chunkmode :tail-adjust-face)))
        (t (oref pm/chunkmode :adjust-face))))

(provide 'polymode-methods)
