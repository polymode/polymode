;;; polymode-classes.el --- Core polymode classes -*- lexical-binding: t -*-
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


(require 'eieio)
(require 'polymode-core)

(setq eieio-backward-compatibility nil)

(defun pm--filter-slots (slots)
  (delq nil (mapcar (lambda (slot)
                      (unless (or (= (elt (symbol-name slot) 0) ?-)
                                  (eq slot 'minor-mode)
                                  (eq slot 'parent-instance)
                                  (eq slot 'object-name))
                        (intern (concat ":" (symbol-name slot)))))
                    slots)))

(if (fboundp 'eieio-named)
    (progn
      ;; bug #22840
      (cl-defmethod clone ((obj eieio-named) &rest params)
        "Clone OBJ, initializing `:parent' to OBJ.
All slots are unbound, except those initialized with PARAMS."
        (let* ((newname (and (stringp (car params)) (pop params)))
               (nobj (apply #'cl-call-next-method obj params))
               (nm (slot-value obj 'object-name)))
          (eieio-oset nobj 'object-name
                      ;;^^-- in emacs proper is obj (:fixme push to emacs)
                      (or newname
                          (save-match-data
                            (if (and nm (string-match "-\\([0-9]+\\)" nm))
                                (let ((num (1+ (string-to-number
                                                (match-string 1 nm)))))
                                  (concat (substring nm 0 (match-beginning 0))
                                          "-" (int-to-string num)))
                              (concat nm "-1")))))
          nobj))

      (defclass pm-root (eieio-named eieio-instance-inheritor)
        ((-props
          :initform '()
          :type list
          :documentation "Internal. Used to store various user
    history values. Use `pm--prop-get' and `pm--prop-put' to
    place key value pairs into this list."))
        "Root polymode class."))

  (defclass pm-root (eieio-instance-inheritor)
    ((-props
      :initform '()
      :type list
      :documentation "[Internal] Plist used to store various extra metadata such as user history. Use `pm--prop-get' and `pm--prop-put' to place key value pairs into this list."))

    "Root polymode class."))

(defclass pm-polymode (pm-root)
  ((hostmode
    :initarg :hostmode
    :initform nil
    :type symbol
    :custom symbol
    :documentation
    "Symbol pointing to a `pm-host-chunkmode' object.")
   (innermodes
    :initarg :innermodes
    :type list
    :initform nil
    :custom (repeat symbol)
    :documentation
    "List of names of the `pm-inner-chunkmode' objects associated with this polymode.")
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
    :documentation "Current exporter name. If non-nil should be the name of the default exporter for this polymode. Can be set with `polymode-set-exporter' command.")
   (weavers
    :initarg :weavers
    :initform '()
    :type list
    :custom (repeat symbol)
    :documentation "List of names of polymode weavers available for this polymode.")
   (weaver
    :initarg :weaver
    :initform nil
    :type symbol
    :documentation "Current weaver name. If non-nil this is the default weaver for this polymode. Can be dynamically set with `polymode-set-weaver'")
   (switch-buffer-functions
    :initarg :switch-buffer-functions
    :initform '()
    :custom (repeat symbol)
    :type list
    :documentation "List of functions to run at polymode buffer switch. Each function is run with two arguments, OLD-BUFFER and NEW-BUFFER.")

   (keylist
    :initarg :keylist
    :initform 'polymode-minor-mode-map
    :type (or symbol list)
    :documentation "A list of elements of the form (KEY . BINDING) which is inherited by child polymodes. This slot is reserved for building hierarchies through cloning and should not be used in `define-polymode'.")

   (minor-mode
    :initarg :minor-mode
    :initform 'polymode-minor-mode
    :type symbol
    :documentation  "[Internal] Symbol pointing to minor-mode function that should be activated in all buffers (base and indirect).")

   (-hostmode
    :type (or null pm-chunkmode)
    :documentation "[Dynamic] Dynamically populated `pm-chunkmode' object.")
   (-innermodes
    :type list
    :initform '()
    :documentation "[Dynamic] Dynamically populated list of chunkmodes objects that inherit from `pm-chunkmode'.")
   (-auto-innermodes
    :type list
    :initform '()
    :documentation "[Dynamic] List of chunkmode objects that are auto-generated by `pm-get-span' methods of auto chunkmodes.")
   (-buffers
    :initform '()
    :type list
    :documentation "[Dynamic] Holds all buffers associated with current buffer. Dynamically populated."))

  "Configuration for a polymode. Each polymode buffer contains a
local variable `pm/polymode' instantiated from this class or a
subclass of this class.")

(defvar pm--polymode-slots
  (mapcar #'cl--slot-descriptor-name
          (eieio-class-slots 'pm-polymode)))


(defclass pm-chunkmode (pm-root)
  ((mode
    :initarg :mode
    :type symbol
    :initform nil)
   (protect-indent-line
    :initarg :protect-indent-line
    :type boolean
    :initform t
    :documentation "Whether to modify local `indent-line-function' by narrowing to current span first")
   (indent-offset
    :initarg :indent-offset
    :type integer
    :initform 0
    :documentation "Offset to add when indenting chunk's line. Takes effect only when :protect-indent-line is non-nil.")
   (font-lock-narrow
    :initarg :font-lock-narrow
    :type boolean
    :initform t
    :documentation "Whether to narrow to span during font lock.")
   (adjust-face
    :initarg :adjust-face
    :type (or number face list)
    ;; :custom (or number face list)
    :initform '()
    :documentation "Fontification adjustments chunk face. It should be either, nil, number, face or a list of text properties as in `put-text-property' specification. If nil no highlighting occurs. If a face, use that face. If a number, it is a percentage by which to lighten/darken the default chunk background. If positive - lighten the background on dark themes and darken on light thems. If negative - darken in dark thems and lighten in light thems.")
   (init-functions
    :initarg :init-functions
    :initform '()
    :type list
    :documentation "List of functions to called after the initialization of  chunkmode has finished. Functions are called the buffer associated with this chunkmode. All init-functions in the inheritance chain are called. Parents hooks first. So, if current config object C inherits from object B, which in turn inherits from object A. Then A's init-functions are called first, then B's and then C's. Either customize this slot or use `object-add-to-list' function.")
   (switch-buffer-functions
    :initarg :switch-buffer-functions
    :initform '()
    :type list
    :documentation "List of functions to run at polymode buffer switch. Each function is run with two arguments, OLD-BUFFER and NEW-BUFFER. In contrast to identically named slot in `pm-polymode' class, these functions are run only when NEW-BUFFER is of this chunkmode.")

   (-buffer
    :type (or null buffer)
    :initform nil))

  "Generic chunkmode object.")

(defclass pm-host-chunkmode (pm-chunkmode)
  ()
  "This chunkmode doesn't know how to compute spans and takes
over all the other space not claimed by other chunkmodes in the
buffer.")

(defclass pm-inner-chunkmode (pm-chunkmode)
  ((head-mode
    :initarg :head-mode
    :type symbol
    :initform 'poly-head-tail-mode
    :documentation "Chunk's header mode. If set to 'body, the head is considered part of the chunk body. If set to 'host, head is considered part of the surrounding host mode.")
   (tail-mode
    :initarg :tail-mode
    :type symbol
    :initform nil
    :documentation "Chunk's tail mode. If nil, or 'head, the mode is picked from :HEAD-MODE slot. If set to 'body or 'host the tail's mode is the same as chunk's body or host mode.")
   (head-matcher
    :initarg :head-matcher
    :initform nil
    :type (or string symbol cons)
    :documentation "A regexp, a cons (REGEXP . SUB-MATCH) or a function. When a function, the matcher must accept one argument that can take either values 1 (forwards search) or -1 (backward search). This function must return either nil (no match) or a (cons BEG END) representing the span of the head or tail respectively. See the code of `pm-fun-matcher' for a simple example.")
   (tail-matcher
    :initarg :tail-matcher
    :initform nil
    :type (or string symbol cons)
    :documentation "Like :head-matcher but for the chunk's tail. It is always called with the point at the end of the matched head and with the positive argument.")
   (adjust-face
    :initform 2)
   (head-adjust-face
    :initarg :head-adjust-face
    :initform font-lock-type-face
    :type (or null number face list)
    :documentation "Can be a number, a list of properties or a face.")
   (tail-adjust-face
    :initarg :tail-adjust-face
    :initform nil
    :type (or null number face list)
    :documentation "Can be nil, a number, a list of properties or a face. When nil, take the configuration from :head-adjust-face.")

   (-head-buffer
    :type (or null buffer)
    :initform nil
    :documentation "[internal] This buffer is set automatically to -buffer if :head-mode is 'body, and to base-buffer if :head-mode is 'host")
   (-tail-buffer
    :initform nil
    :type (or null buffer)
    :documentation "[internal] Same as -head-buffer, but for tail span."))

  "Inner-chunkmodes represent innermodes (or sub-modes) within a
buffer. Chunks are commonly delimited by head and tail markup but
can be delimited by some other logic (e.g. indentation). In the
latter case, heads or tails have zero length and are not
physically present in the buffer.")

(defclass pm-inner-auto-chunkmode (pm-inner-chunkmode)
  ((mode-matcher
    :initarg :mode-matcher
    :type (or null string symbol cons)
    :initform nil
    :documentation "Matcher used to retrive the mode's symbol from the chunk's head. Can be either a regexp string, cons of the form (REGEXP . SUBEXPR) or a function to be called with no arguments. If a function, it must return a string name of the mode. The matcher function is called with point at the start of the head span."))

  "Inner chunkmodes with unknown (at definition time) mode of the
body span. The body mode is determined dynamically by retrieving
the name with the :mode-matcher.")

;; fixme: should be let or return to the prev value no t
(setq eieio-backward-compatibility t)

(provide 'polymode-classes)
