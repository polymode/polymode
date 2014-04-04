
;;; ROOT CLASS
(defclass polymode (eieio-instance-inheritor) ()
  "Root polymode class.")


;;; CONFIG
(defclass pm-config (polymode) 
  ((basemode-name
    :initarg :basemode-name
    :initform 'pm-base/blank
    :type symbol
    :custom symbol
    :documentation
    "Symbol pointing to an object of class pm-submode
    representing the base submode.")
   (minor-mode-name
    :initarg :minor-mode-name
    :initform 'polymode-minor-mode
    :type symbol
    :custom symbol
    :documentation
    "Symbol pointing to minor-mode function that should be
    activated in all buffers (base and indirect). This is a
    \"glue\" mode and is `polymode-minor-mode' by default.")
   (lighter
    :initarg :lighter
    :initform " PM"
    :type string
    :custom string
    :documentation "Modline lighter.")
   (basemode
    :initarg :basemode
    :type (or null pm-submode)
    :documentation
    "Instantiated submode")
   (innermodes
    :initarg :innermodes
    :type list
    :initform '()
    :documentation
    "List of submodes objects that inherit from `pm-innermode'")
   (buffers
    :initarg :buffers
    :initform '()
    :type list
    :documentation
    "Holds all buffers associated with current buffer")
   (map
    :initarg :map
    :initform 'polymode-mode-map
    :type (or symbol list)
    "Has a similar role as the :keymap argument in `define-polymode'
with the difference that this argument is inherited through
cloning but :keymap argument is not. That is, child objects
derived through clone will inherit the :map argument of its
parents as follows. If :map is nil or an alist of keys, the
parent is inspected for :map argument and the keys are
merged. If :map is a symbol, it should be a keymap, in which case
this keymap is used and no parents are further inspected for :map
slot. If :map is an alist it should be suitable to be passed to
`easy-mmode-define-keymap'.")
   (init-functions
    :initarg :init-functions
    :initform '()
    :type list
    :documentation
    "List of functions to run at the initialization time.
All init-functions in the inheritance chain are called. Parents
hooks first. So, if current config object C inherits from object
B, which in turn inherits from object A. Then A's init-functions
are called first, then B's and then C's.

Either customize this slot or use `object-add-to-list' function."))
  
  "Configuration for a polymode. Each polymode buffer contains a local
variable `pm/config' instantiated from this class or a subclass
of this class.")


(defclass pm-config-one (pm-config)
  ((innermode-name
    :initarg :innermode-name
    :type symbol
    :custom symbol
    :documentation
    "Symbol of the submode. At run time this object is cloned
     and placed in :innermodes slot."))
  
  "Configuration for a simple polymode that allows only one
submode. For example noweb.")


(defclass pm-config-multi (pm-config)
  ((innermode-names
    :initarg :innermode-names
    :type list
    :custom list
    :initform nil
    :documentation
    "List of names of the submode objects that are associated
     with this configuration. At initialization time, all of
     these are cloned and plased in :innermodes slot."))
  
  "Configuration for a polymode that allows multiple known in
advance submodes.")


(defclass pm-config-multi-auto (pm-config-multi)
  ((auto-submode-name
    :initarg :auto-submode-name
    :type symbol
    :custom symbol
    :documentation
    "Name of auto-submode (a symbol). At run time this object is
     cloned and placed in :auto-submodes with coresponding :mode
     slot initialized at run time.")
   (auto-submodes
    :initarg :auto-submodes
    :type list
    :initform '()
    :documentation
    "List of submodes that are auto-generated in pm/get-span
    method for this class."))
  
  "Configuration for a polymode that allows multiple submodes
that are not known in advance. Examples are org-mode and markdown.")



;;; SUBMODE CLASSES
(defclass pm-submode (polymode)
  ((mode
    :initarg :mode
    :type symbol
    :initform nil
    :custom symbol)
   (protect-indent-line
    :initarg :protect-indent-line
    :type boolean
    :initform t
    :custom boolean
    :documentation
    "Whether to modify local `indent-line-function' by narrowing
    to current span first")
   (indent-offset
    :initarg :indent-offset
    :type integer
    :initform 0
    :documentation
    "Offset to add when indenting chunk's line. Takes efeect only
    when :protect-indent-line is non-nil.")
   (font-lock-narrow
    :initarg :font-lock-narrow
    :type boolean
    :initform t
    :documentation
    "Whether to narrow to span during font lock")
   (buffer
    :initarg :buffer
    :type (or null buffer)
    :initform nil)
   (adjust-face
    :initarg :adjust-face
    :type (or number face list)
    :custom (or number face list)
    :initform nil
    :documentation
    "Fontification adjustments chunk face. It should be either,
    nil, number, face or a list of text properties as in
    `put-text-property' specification.

    If nil no highlighting occurs. If a face, use that face. If a
    number, it is a percentage by which to lighten/darken the
    default chunk background. If positive - lighten the
    background on dark themes and darken on light thems. If
    negative - darken in dark thems and lighten in light
    thems."))
  
  "Represents a simple submode. Usually used for the definition
of the base submodes (aka host submodes associated with the base
buffer).")

(defclass pm-innermode (pm-submode)
  ((adjust-face
    :initform 2)
   (head-mode
    :initarg :head-mode
    :type symbol
    :initform 'fundamental-mode
    :custom symbol
    :documentation
    "Chunks' header mode. If set to 'body, the head is considered
    part of the chunk body. If set to 'base, head is considered
    part of the including base mode.")
   (head-buffer
    :initarg :head-buffer
    :type (or null buffer)
    :initform nil
    :documentation
    "This buffer is set automatically to :buffer if :head-mode is
    'body, and to base-buffer if :head-mode is 'base")
   (tail-mode
    :initarg :tail-mode
    :type symbol
    :initform nil
    :custom symbol
    :documentation
    "If nil, it is the same as :HEAD-MODE. Otherwise, the same
    rules as for the :head-mode apply.")
   (tail-buffer
    :initarg :tail-buffer
    :initform nil
    :type (or null buffer))
   (head-reg
    :initarg :head-reg
    :initform ""
    :type (or string symbol)
    :custom (or string symbol)
    :documentation "Regexp for the chunk start (aka head)")
   (tail-reg
    :initarg :tail-reg
    :initform ""
    :type (or string symbol)
    :custom (or string symbol)
    :documentation "Regexp for chunk end (aka tail)")
   (head-adjust-face
    :initarg :head-adjust-face
    :initform font-lock-type-face
    :type (or null number face list)
    :custom (or null number face list)
    :documentation
    "Can be a number, list or face.")
   (tail-adjust-face
    :initarg :tail-adjust-face
    :initform nil
    :type (or null number face list)
    :custom (or null number face list)
    :documentation
    "Can be a number, list or face. If nil, take the
configuration from :head-adjust-face."))
  
  "Representation of an inner (aka child) submode in a buffer.")

(defclass pm-innermode-auto (pm-innermode)
  ((retriever-regexp
    :initarg :retriever-regexp
    :type (or null string)
    :custom string
    :initform nil
    :documentation
    "Regexp that is used to retrive the modes symbol from the
    head of the submode chunk. fixme: elaborate")
   (retriever-num
    :initarg :retriever-num
    :type integer
    :custom integer
    :initform 1
    :documentation
    "Subexpression to be matched by :retriver-regexp")
   (retriever-function
    :initarg :retriever-function
    :type symbol
    :custom symbol
    :initform nil
    :documentation
    "Function name that is used to retrive the modes symbol from
    the head of the submode chunk. fixme: elaborate"))

  "Representation of an inner submode")



(provide 'polymode-classes)
