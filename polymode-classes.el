;;; polymode-classes.el --- Core polymode classes -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013-2019, Vitalie Spinu
;; Author: Vitalie Spinu
;; URL: https://github.com/polymode/polymode
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;; Code:

(require 'eieio)
(require 'eieio-base)
(require 'eieio-custom)

(defun pm--unbind-and-set-slots (obj ancestor params)
  (dolist (descriptor (eieio-class-slots (eieio-object-class ancestor)))
    (let ((slot (eieio-slot-descriptor-name descriptor)))
      (unless (memq slot '(parent name))
        (slot-makeunbound obj slot))))
  (when params
    (shared-initialize obj params))
  obj)

(defun pm--instantiate-from-ancestor (class ancestor params)
  ;; Called from make-instance and should not call constructors!
  ;; Relying on eieio internal cache implementation.
  (let* ((obj (copy-sequence (eieio--class-default-object-cache
                              (eieio--class-object class))))
         (obj (pm--unbind-and-set-slots obj ancestor params)))
    ;; ":" indicates that object's names is derived from the parent name
    (unless (plist-get params :name)
      (let ((old-name (eieio-oref ancestor 'name)))
        (eieio-oset obj 'name (concat old-name ":"))))
    (aset obj 0 class)
    obj))

;; fixme: params as list
(defun pm--safe-clone (class obj &rest params)
  "Clone to an object of class CLASS.
If CLASS is same as class of OBJ then just call `clone'.
Otherwise do a bit more work by setting extra slots of the new
object. PARAMS are passed to clone or constructor functions."
  (if (eq class (eieio-object-class obj))
      (apply #'clone obj params)
    (pm--instantiate-from-ancestor class obj params)))

;; We are no longer relying on eieio-instance-inheritor from v0.3 for a bunch of
;; reasons:
;;
;;  - Cloning of eieio-instance-inheritor was broken prior to emacs 28
;;
;;  - :parent-instance copies are copies but we want them to be symbols to be
;;    dynamically de-referenced on slot lookup.
;;
;;  - We want the entire hierarchy to be inheritable through objects. If a root
;;    object is modified, the modification should propagate to all children. In
;;    other words, if the user customizes `poly-root-chunkmode' the change
;;    should be immediately visible to all innermodes and hostmodes.
(defclass pm-root ()
  ((name
    :initarg :name
    :initform "UNNAMED"
    :type string
    :custom string
    :documentation
    "Name of the object used to for display and info.")
   (parent
    :initarg :parent
    :initform 'poly-root
	:type symbol
	:documentation
	"Name of the parent of this instance (a symbol).
When a slot of an object is not set the parent is checked for a
value.")

   (-props
    :initform '()
    :type list
    :documentation
    "[Internal] Plist used to store various extra metadata such as user history.
Use `pm--prop-get' and `pm--prop-put' to place key value pairs
into this list."))

  "Root polymode class.")

(cl-defmethod slot-unbound ((object pm-root) _class slot-name _fn)
  (if (slot-boundp object 'parent)
      ;; recur back into this method if the parent's slot is unbound
      (eieio-oref (symbol-value (eieio-oref object 'parent)) slot-name)
    ;; Throw the regular signal.
    (cl-call-next-method)))

(cl-defmethod eieio-object-name-string ((obj pm-root))
  (eieio-oref obj 'name))

(cl-defmethod clone ((obj pm-root) &rest params)
  (let ((new-obj (cl-call-next-method obj)))
    (pm--unbind-and-set-slots new-obj obj params)))

(cl-defmethod make-instance ((class (subclass pm-root)) &rest args)
  (if (eq class 'pm-root)
      (cl-call-next-method)
    (let* ((name (plist-get args :name))
           (parent (or (plist-get args :parent)
                       (eieio-oref-default class 'parent))))
      (when (equal name (symbol-name parent))
        ;; Special case to allow defining root objects with this
        ;; constructor.
        (setq parent (eieio-oref-default (eieio-class-name (eieio-class-parent class))
                                         'parent)))
      (pm--instantiate-from-ancestor class (symbol-value parent) args))))

(defclass pm-polymode (pm-root)
  ((parent
    :initform 'poly-root-polymode)
   (hostmode
    :initarg :hostmode
    :initform nil
    :type symbol
    :custom symbol
    :documentation
    "Symbol pointing to a `pm-hostmode' object.
When nil, any host-mode will be matched (suitable for
poly-minor-modes. ")
   (innermodes
    :initarg :innermodes
    :type list
    :initform nil
    :custom (repeat symbol)
    :documentation
    "List of inner-mode names (symbols) associated with this polymode.
A special marker :inherit in this list is replaced with the
innermodes of the parent. This allows for a simple way to add
innermodes to the child without explicitly listing all the
innermodes of the parent.")
   (exporters
    :initarg :exporters
    :initform '(pm-exporter/pandoc)
    :custom (repeat symbol)
    :documentation
    "List of names of polymode exporters available for this polymode.")
   (exporter
    :initarg :exporter
    :initform nil
    :type symbol
    :custom symbol
    :documentation
    "Current exporter name.
If non-nil should be the name of the default exporter for this
polymode. Can be set with `polymode-set-exporter' command.")
   (weavers
    :initarg :weavers
    :initform '()
    :type list
    :custom (repeat symbol)
    :documentation
    "List of names of polymode weavers available for this polymode.")
   (weaver
    :initarg :weaver
    :initform nil
    :type symbol
    :custom symbol
    :documentation
    "Current weaver name.
If non-nil this is the default weaver for this polymode. Can be
dynamically set with `polymode-set-weaver'")
   (switch-buffer-functions
    :initarg :switch-buffer-functions
    :initform '()
    :type list
    :custom (repeat symbol)
    :documentation
    "List of functions to run at polymode buffer switch.
Each function is run with two arguments, OLD-BUFFER and
NEW-BUFFER.")
   (keylist
    :initarg :keylist
    :initform 'polymode-minor-mode-map
    :type (or symbol list)
    :custom (choice (symbol :tag "Keymap")
                    (repeat (cons string symbol)))
    :documentation
    "A list of elements of the form (KEY . BINDING).
This slot is reserved for building hierarchies through cloning
and should not be used in `define-polymode'.")
   (keep-in-mode
    :initarg :keep-in-mode
    :initform nil
    :type symbol
    :custom symbol
    :documentation
    ;; NB: Using major-modes instead of innermode symbols for the sake of
    ;; simplicity of the implementation and to allow for auto-modes.
    "Major mode to keep in when polymode switches implementation buffers.
When a special symbol 'host, keep in hostmode. The buffer with
this major mode must be installed by one of the innermodes or the
hostmode. If multiple innermodes installed buffers of this mode,
the first buffer is used.")

   (-minor-mode
    :initform 'polymode-minor-mode
    :initarg -minor-mode
    :type symbol
    :documentation
    "[Internal] Symbol pointing to minor-mode function.")
   (-hostmode
    :type (or null pm-chunkmode)
    :documentation
    "[Dynamic] Dynamically populated `pm-chunkmode' object.")
   (-innermodes
    :type list
    :initform '()
    :documentation
    "[Dynamic] List of chunkmodes objects.")
   (-auto-innermodes
    :type list
    :initform '()
    :documentation
    "[Dynamic] List of auto chunkmodes.")
   (-buffers
    :initform '()
    :type list
    :documentation
    "[Dynamic] Holds all buffers associated with current buffer."))

  "Polymode Configuration object.
Each polymode buffer holds a local variable `pm/polymode'
instantiated from this class or a subclass of this class.")

(defclass pm-chunkmode (pm-root)
  ((parent
    :initform 'poly-root-chunkmode)
   (mode
    :initarg :mode
    :initform nil
    :type symbol
    :custom symbol
    :documentation
    "Emacs major mode for the chunk's body.
If :mode slot is nil (anonymous chunkmodes), use the value of
`polymode-default-inner-mode' is when set, or use the value of
the slot :fallback-mode. A special value 'host means to use the
host mode (useful auto-chunkmodes only).")
   (fallback-mode
    :initarg :fallback-mode
    :initform 'poly-fallback-mode
    :type symbol
    :custom symbol
    :documentation
    "Mode to use when mode lookup fails for various reasons. Can
    take a special value 'host. Note that, when set,
    `polymode-default-inner-mode' takes precedence over this
    value.")
   (allow-nested
    :initarg :allow-nested
    :initform t
    :type symbol
    :custom symbol
    :documentation
    "Non-nil if other inner-modes are allowed to nest within this
inner-mode.")
   (indent-offset
    :initarg :indent-offset
    :initform 2
    :type (or number symbol)
    :custom (choice number symbol)
    :documentation
    "Indentation offset for this mode.
Currently this is only used in +indent and -indent cookies which
when placed on a line cause manual shift in indentation with
respect to how polymode would normally indent a line. Should be
used in cases when indentation of the line is incorrect. Can be a
number, a variable name or a function name to be called with no
arguments.")
   (pre-indent-offset
    :initarg :pre-indent-offset
    :initform 0
    :type (or number function)
    :custom (choice number function)
    :documentation
    "Function to compute the offset first line of this chunk.
Offset is relative to how the host mode would indent it. Called
with no-arguments with the point at the begging of the chunk.")
   (post-indent-offset
    :initarg :post-indent-offset
    :initform 0
    :type (or number function)
    :custom (choice number function)
    :documentation
    "Function to compute the offset of the following line after this chunk.
Offset is relative to how the host mode would indent it. Called
without arguments with point at the end of the chunk but before
the trailing white spaces if any.")
   (protect-indent
    :initarg :protect-indent
    :initform nil
    :type boolean
    :custom boolean
    :documentation
    "Whether to narrowing to current span before indent.")
   (protect-font-lock
    :initarg :protect-font-lock
    :initform nil
    :type boolean
    :custom boolean
    :documentation
    "Whether to narrow to span during font lock.")
   (protect-syntax
    :initarg :protect-syntax
    :initform nil
    :type boolean
    :custom boolean
    :documentation
    "Whether to narrow to span when calling `syntax-propertize-function'.")
   (adjust-face
    :initarg :adjust-face
    :initform nil
    :type (or number face list)
    :custom (choice number face sexp)
    :documentation
    "Fontification adjustment for the body of the chunk.
It should be either, nil, number, face or a list of text
properties as in `put-text-property' specification. If nil or 0
no highlighting occurs. If a face, use that face. If a number, it
is a percentage by which to lighten/darken the default chunk
background. If positive - lighten the background on dark themes
and darken on light thems. If negative - darken in dark thems and
lighten in light thems.")
   (init-functions
    :initarg :init-functions
    :initform '()
    :type list
    :custom hook
    :documentation
    "List of functions called after the initialization.
Functions are called with one argument TYPE in the buffer
associated with this chunkmode's span. TYPE is either 'host,
'head, 'body or 'tail. All init-functions in the inheritance
chain are called in parent-first order. Either customize this
slot or use `object-add-to-list' function.")
   (switch-buffer-functions
    :initarg :switch-buffer-functions
    :initform '()
    :type list
    :custom hook
    :documentation
    "List of functions to run at polymode buffer switch.
Each function is run with two arguments, OLD-BUFFER and
NEW-BUFFER. In contrast to identically named slot in
`pm-polymode' class, these functions are run only when NEW-BUFFER
is of this chunkmode.")
   (keep-in-mode
    :initarg :keep-in-mode
    :initform nil
    :type symbol
    :custom symbol
    :documentation
    "Major mode to keep in when polymode switches implementation buffers.
When a special symbol 'host, keep in hostmode. The buffer with
this major mode must be installed by one of the innermodes or the
hostmode. If multiple innermodes installed buffers of this mode,
the first buffer is used.")

   (-buffer
    :type (or null buffer)
    :initform nil))

  "Generic chunkmode object.
Please note that by default :protect-xyz slots are nil in
hostmodes and t in innermodes.")

(defclass pm-hostmode (pm-chunkmode)
  ((parent
    :initform 'poly-root-hostmode)
   (allow-nested
    ;; currently ignored in code as it doesn't make sense to not allow
    ;; innermodes in hosts
    :initform 'always))

  "This chunkmode doesn't know how to compute spans and takes
over all the other space not claimed by other chunkmodes in the
buffer.")

(defclass pm-innermode (pm-chunkmode)
  ((parent
    :initform 'poly-root-innermode)
   (protect-font-lock
    :initform t)
   (protect-syntax
    :initform t)
   (protect-indent
    :initform t)
   (body-indent-offset
    :initarg :body-indent-offset
    :initform 0
    :type (or number symbol function)
    :custom (choice number symbol)
    :documentation
    "Indentation offset of the body span relative to the head.
Can be a number, symbol holding a number or a function. When a
function, it is called with no arguments at the beginning of the
body span.")
   (can-nest
    :initarg :can-nest
    :initform nil
    :type boolean
    :custom boolean
    :documentation
    "Non-nil if this inner-mode can nest within other inner-modes.
All chunks can nest within the host-mode.")
   (can-overlap
    :initarg :can-overlap
    :initform nil
    :type boolean
    :custom boolean
    :documentation
    "Non-nil if chunks of this type can overlap with other chunks of the same type.
See noweb for an example.")
   (head-mode
    :initarg :head-mode
    :initform 'poly-head-tail-mode
    :type symbol
    :custom symbol
    :documentation
    "Chunk's head mode.
If set to 'host or 'body use host or body's mode respectively.")
   (tail-mode
    :initarg :tail-mode
    :initform 'poly-head-tail-mode
    :type symbol
    :custom (choice (const nil :tag "From Head")
                    function)
    :documentation
    "Chunk's tail mode.
If set to 'host or 'body use host or body's mode respectively.")
   (head-matcher
    :initarg :head-matcher
    :type (or string cons function)
    :custom (choice string (cons string integer) function)
    :documentation
    "A REGEXP, a cons (REGEXP . SUB-MATCH) or a function.
When a function, the matcher must accept one argument that can
take either values 1 (forwards search) or -1 (backward search)
and behave similarly to how search is performed by
`re-search-forward' function. This function must return either
nil (no match) or a (cons BEG END) representing the head span.
See the code of `pm-fun-matcher' for how REGEXP and (REGEXP .
SUB-MATCH) are converted to a function internally..")
   (tail-matcher
    :initarg :tail-matcher
    :type (or string cons function)
    :custom (choice string (cons string integer) function)
    :documentation
    "A regexp, a cons (REGEXP . SUB-MATCH) or a function.
Like :head-matcher but for the chunk's tail. Unlike
:head-matcher, it is always called with the point at the end of
the matched head and with the positive argument (aka match
forward). See `pm-forward-sexp-tail-matcher' for an example.")
   (adjust-face
    :initform 2)
   (head-adjust-face
    :initarg :head-adjust-face
    :initform 'bold
    :type (or number face list)
    :custom (choice number face sexp)
    :documentation
    "Head's face adjustment.
Can be a number, a list of properties or a face.")
   (tail-adjust-face
    :initarg :tail-adjust-face
    :initform nil
    :type (or null number face list)
    :custom (choice (const :tag "From Head" nil)
                    number face sexp)
    :documentation
    "Tail's face adjustment.
A number, a list of properties, a face or nil. When nil, take the
configuration from :head-adjust-face.")

   (-head-buffer
    :type (or null buffer)
    :initform nil
    :documentation
    "[Internal] This buffer is set automatically to -buffer if
:head-mode is 'body, and to base-buffer if :head-mode is 'host.")
   (-tail-buffer
    :initform nil
    :type (or null buffer)
    :documentation
    "[Internal] Same as -head-buffer, but for tail span."))

  "Inner-chunkmodes represent innermodes (or sub-modes) within a
buffer. Chunks are commonly delimited by head and tail markup but
can be delimited by some other logic (e.g. indentation). In the
latter case, heads or tails have zero length and are not
physically present in the buffer.")

(defclass pm-auto-innermode (pm-innermode)
  ((parent
    :initform 'poly-root-auto-innermode)
   (mode-matcher
    :initarg :mode-matcher
    :type (or string cons function)
    :custom (choice string (cons string integer) function)
    :documentation
    "Matcher used to retrieve the mode's symbol from the chunk's head.
Can be either a regexp string, cons of the form (REGEXP .
SUBEXPR) or a function to be called with no arguments. If a
function, it must return a string name of the mode. Function is
called at the beginning of the head span."))

  "Inner chunkmodes with unknown (at definition time) mode of the
body span. The body mode is determined dynamically by retrieving
the name with the :mode-matcher.")

;; Obsolete old names
(defclass pm-host-chunkmode (pm-hostmode) ()
  "Obsolete. Use class `pm-hostmode' instead.")
(defclass pm-inner-chunkmode (pm-innermode) ()
  "Obsolete. Use class `pm-innermode' instead.")
(defclass pm-inner-auto-chunkmode (pm-auto-innermode) ()
  "Obsolete. Use class `pm-auto-innermode' instead.")


;;; Root Object Hierarchy
;;
;; We want users to be able to customize objects globally by using the unbound
;; slot inheritance. But this is not possible with `oset-default' for two
;; reasons:
;;
;;    1) unbound slot inheritance propagates through object hierarchy but not
;;       through class hierarchy.
;;
;;    2) With oset-default users would need to add their customization before
;;       chunkmodes are instantiated and this is imposible for built-in chunk
;;       modes defined in poly-base.el.
;;
;; Therefore, we build a parallel object hierarchy which follows the class
;; hierarchy with pm--safe-clone and let the eieio classes be simply an
;; implementation detail.

(defvar poly-root (pm-root :name "poly-root")
  "Parent config of all polymode objects.")

(defvar poly-root-polymode (pm-polymode :name "poly-root-polymode")
  "Parent config of all polymodes.")

(defvar poly-root-chunkmode (pm-chunkmode :name "poly-root-chunkmode")
  "Parent config for all hostmodes and innermodes.")

(defvar poly-root-hostmode (pm-hostmode :name "poly-root-hostmode")
  "Parent config for all hostmodes.")

(defvar poly-root-innermode (pm-innermode :name "poly-root-innermode")
  "Parent config for all innermodes.")

(defvar poly-root-auto-innermode (pm-auto-innermode :name "poly-root-auto-innermode")
  "Parent config for all auto innermodes.")

(provide 'polymode-classes)
;;; polymode-classes.el ends here


(pm-hostmode :name "blabal")
