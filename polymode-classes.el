(require 'eieio)
(require 'polymode-core)

;;; ROOT CLASS
(if (fboundp 'eieio-named)
    (progn
      (defclass pm-root (eieio-instance-inheritor eieio-named)
        ((-props
          :initform '()
          :type list
          :documentation "Internal. Used to store various user
    history values. Use `pm--prop-get' and `pm--prop-put' to
    place key value pairs into this list."))
        "Root polymode class.")

      (when (fboundp 'defmethod)
        ;; bug #22840
        (defmethod clone ((obj eieio-named) &rest params)
          "Clone OBJ, initializing `:parent' to OBJ.
        All slots are unbound, except those initialized with
        PARAMS."
          (let* ((newname (and (stringp (car params)) (pop params)))
                 (nobj (apply #'call-next-method obj params))
                 (nm (slot-value obj 'object-name)))
            (eieio-oset nobj 'object-name
                        (or newname
                            (save-match-data
                              (if (and nm (string-match "-\\([0-9]+\\)" nm))
                                  (let ((num (1+ (string-to-number
                                                  (match-string 1 nm)))))
                                    (concat (substring nm 0 (match-beginning 0))
                                            "-" (int-to-string num)))
                                (concat nm "-1")))))
            nobj))))

  (defclass pm-root (eieio-instance-inheritor)
    ((-props
      :initform '()
      :type list
      :documentation "Internal. Plist used to store various extra
    metadata such as user history. Use `pm--prop-get' and
    `pm--prop-put' to place key value pairs into this list."))
    "Root polymode class."))

;;; CONFIG
(defclass pm-polymode (pm-root)
  ((hostmode
    :initarg :hostmode
    :initform 'pm-host/blank
    :type symbol
    :custom symbol
    :documentation
    "Symbol pointing to an object of class pm-chunkmode
    representing the host chunkmode.")
   (minor-mode
    :initarg :minor-mode
    :initform 'polymode-minor-mode
    :type symbol
    :custom symbol
    :documentation
    "Symbol pointing to minor-mode function that should be
    activated in all buffers (base and indirect). This is a
    \"glue\" mode and is `polymode-minor-mode' by default. You
    will rarely need to change this.")
   (lighter
    :initarg :lighter
    :initform " PM"
    :type string
    :custom string
    :documentation "Modline lighter.")
   (exporters
    :initarg :exporters
    :initform '(pm-exporter/pandoc)
    :type list
    :custom list
    :documentation
    "List of names of polymode exporters available for this polymode.")
   (exporter
    :initarg :exporter
    :initform nil
    :type (or null symbol)
    :custom symbol
    :documentation
    "Current exporter name. If non-nil should be the name of the
    default exporter for this polymode. Can be set with
    `polymode-set-exporter' command.")
   (weavers
    :initarg :weavers
    :initform '()
    :type list
    :custom list
    :documentation
    "List of names of polymode weavers available for this polymode.")
   (weaver
    :initarg :weaver
    :initform nil
    :type (or null symbol)
    :custom symbol
    :documentation
    "Current weaver name. If non-nil this is the default weaver
    for this polymode. Can be dynamically set with
    `polymode-set-weaver'")
   (map
    :initarg :map
    :initform 'polymode-mode-map
    :type (or symbol list)
    :documentation
    "Has a similar role as the :keymap argument in
     `define-polymode' with the difference that this argument is
     inherited through cloning, but :keymap argument is not. That
     is, child objects derived through clone will inherit
     the :map argument of its parents through the following
     scheme: if :map is nil or an alist of keys, the parent is
     inspected for :map argument and the keys are merged
     recursively from parent to parent till a symbol :map slot is
     met. If :map is a symbol, it must be a keymap, in which case
     this keymap is used and no parents are further inspected
     for :map slot. If :map is an alist it must be suitable for
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
     Either customize this slot or use `object-add-to-list' function.")
   (switch-buffer-functions
    :initarg :switch-buffer-functions
    :initform '()
    :type list
    :documentation
    "List of functions to run at polymode buffer switch.
     Each function is run with two arguments, OLD-BUFFER and
     NEW-BUFFER.")

   (-hostmode
    :type (or null pm-chunkmode)
    :documentation
    "Dynamically populated `pm-chunkmode' object.")
   (-innermodes
    :type list
    :initform '()
    :documentation
    "Dynamically populated list of chunkmodes objects that
    inherit from `pm-hbtchunkmode'.")
   (-buffers
    :initform '()
    :type list
    :documentation
    "Holds all buffers associated with current buffer. Dynamically populated."))

  "Configuration for a polymode. Each polymode buffer contains a local
variable `pm/polymode' instantiated from this class or a subclass
of this class.")

(defclass pm-polymode-one (pm-polymode)
  ((innermode
    :initarg :innermode
    :type symbol
    :custom symbol
    :documentation
    "Symbol of the chunkmode. At run time this object is cloned
     and placed in -innermodes slot."))

  "Configuration for a simple polymode that allows only one
innermode. For example noweb.")

(defclass pm-polymode-multi (pm-polymode)
  ((innermodes
    :initarg :innermodes
    :type list
    :custom list
    :initform nil
    :documentation
    "List of names of the chunkmode objects that are associated
     with this configuration. At initialization time, all of
     these are cloned and plased in -innermodes slot."))

  "Configuration for a polymode that allows multiple (known in
advance) innermodes.")

(defclass pm-polymode-multi-auto (pm-polymode-multi)
  ((auto-innermode
    :initarg :auto-innermode
    :type symbol
    :custom symbol
    :documentation
    "Name of pm-hbtchunkmode-auto object (a symbol). At run time
     this object is cloned and placed in -auto-innermodes with
     coresponding :mode slot initialized at run time.")
   (-auto-innermodes
    :type list
    :initform '()
    :documentation
    "List of chunkmode objects that are auto-generated in
    `pm-get-span' method for this class."))

  "Configuration for a polymode that allows multiple innermodes
that are not known in advance. Examples are org-mode and markdown.")


;;; CHUNKMODE CLASSES
(defclass pm-chunkmode (pm-root)
  ((mode :initarg :mode
     :type symbol
     :initform nil
     :custom symbol)
   (protect-indent-line :initarg :protect-indent-line
            :type boolean
            :initform t
            :custom boolean
            :documentation
            "Whether to modify local `indent-line-function' by narrowing
    to current span first")
   (indent-offset :initarg :indent-offset
          :type integer
          :initform 0
          :documentation
          "Offset to add when indenting chunk's line. Takes effect only
    when :protect-indent-line is non-nil.")
   (font-lock-narrow :initarg :font-lock-narrow
             :type boolean
             :initform t
             :documentation
             "Whether to narrow to span during font lock")
   (adjust-face :initarg :adjust-face
        :type (or number face list)
        :custom (or number face list)
        :initform nil
        :documentation
        "Fontification adjustments chunk face. It should be either,
    nil, number, face or a list of text properties as in
    `put-text-property' specification. If nil no highlighting
    occurs. If a face, use that face. If a number, it is a
    percentage by which to lighten/darken the default chunk
    background. If positive - lighten the background on dark
    themes and darken on light thems. If negative - darken in
    dark thems and lighten in light thems.")
   (init-functions
    :initarg :init-functions
    :initform '()
    :type list
    :documentation
    "List of functions to called after the initialization of  chunkmode has finished.
     Functions are called the buffer associated with this
     chunkmode. All init-functions in the inheritance chain are
     called. Parents hooks first. So, if current config object C
     inherits from object B, which in turn inherits from object
     A. Then A's init-functions are called first, then B's and
     then C's. Either customize this slot or use
     `object-add-to-list' function.")
   (switch-buffer-functions
    :initarg :switch-buffer-functions
    :initform '()
    :type list
    :documentation
    "List of functions to run at polymode buffer switch.
     Each function is run with two arguments, OLD-BUFFER and
     NEW-BUFFER. In contrast to identically named slot in
     `pm-polymode' class, these functions are run only when
     NEW-BUFFER is associated with this chunkmode.")

   (-buffer
    :type (or null buffer)
    :initform nil))

  "Representatioin of a generic chunkmode object.")

(defclass pm-bchunkmode (pm-chunkmode)
  ()
  "Representation of the body-only chunkmodes. Body-only
  chunkmodes are commonly used as host modes. For example for a
  the web-mdoe the hostmode is `html-mode', for nowweb mode the
  host mode is usually `latex-mode', etc.")

(defclass pm-hbtchunkmode (pm-chunkmode)
  ((head-mode
    :initarg :head-mode
    :type symbol
    :initform 'poly-head-tail-mode
    :custom symbol
    :documentation
    "Chunk's header mode. If set to 'body, the head is considered
    part of the chunk body. If set to 'host, head is considered
    part of the surrounding host mode.")
   (tail-mode
    :initarg :tail-mode
    :type symbol
    :initform nil
    :custom symbol
    :documentation
    "Chunk's tail mode. If nil, or 'head, the mode is picked
    from :HEAD-MODE slot. If set to 'body, the tail's mode is the
    same as chunk's body mode. If set to 'host, the mode will be
    of the parent host.")

   (head-reg
    :initarg :head-reg
    :initform ""
    :type (or string symbol)
    :custom (or string symbol)
    :documentation "Regexp for the chunk start (aka head), or a
    function returning the start and end positions of the head.
    See `pm--default-matcher' for an example function.")
   (tail-reg
    :initarg :tail-reg
    :initform ""
    :type (or string symbol)
    :custom (or string symbol)
    :documentation "Regexp for chunk end (aka tail), or a
    function returning the start and end positions of the tail.
    See `pm--default-matcher' for an example function.")

   (adjust-face
    :initform 2)
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
    configuration from :head-adjust-face.")

   (-head-buffer
    :type (or null buffer)
    :initform nil
    :documentation
    "This buffer is set automatically to -buffer if :head-mode is
    'body, and to base-buffer if :head-mode is 'host")
   (-tail-buffer
    :initform nil
    :type (or null buffer)))

  "Representation of an inner Head-Body-Tail chunkmode.")

(defclass pm-hbtchunkmode-auto (pm-hbtchunkmode)
  ((retriever-regexp :initarg :retriever-regexp
             :type (or null string)
             :custom string
             :initform nil
             :documentation
             "Regexp that is used to retrive the modes symbol from the
    head of the chunkmode chunk. fixme: elaborate")
   (retriever-num :initarg :retriever-num
          :type integer
          :custom integer
          :initform 1
          :documentation
          "Subexpression to be matched by :retriver-regexp")
   (retriever-function :initarg :retriever-function
               :type symbol
               :custom symbol
               :initform nil
               :documentation
               "Function symbol used to retrive the modes symbol from the
    head of the chunkmode chunk. It is called with no arguments
    with the point positioned at the beginning of the chunk
    header. It must return the mode name string or symbol (need
    not include '-mode' postfix).)"))

  "Representation of an inner chunkmode")

(provide 'polymode-classes)
