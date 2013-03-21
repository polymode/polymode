;;; ROOT CLASS
(defclass polymode (eieio-instance-inheritor) ()
  "Root polymode class.")

;;; CONFIG
(defclass pm-config (polymode) 
  ((base-submode-name :initarg :base-submode-name
                      :initform 'pm-base/fundamental
                      :type symbol
                      :custom symbol
                      :documentation
                      "Symbol pointing to an object of class
                      pm-submode representing the base submode.")
   (minor-mode-name :initarg :minor-mode-name
                    :initform 'polymode-minor-mode
                    :type symbol
                    :custom symbol
                    :documentation
                    "Symbol pointing to minor-mode function that
                    should be activated in all buffers (base and
                    indirect). This is a \"glue\" mode and is
                    `polymode-minor-mode' by default.")
   (base-submode :initarg :base-submode
                 :type (or null pm-submode)
                 :documentation
                 "Instantiated submode")
   (inner-submodes :initarg :inner-submodes
                   :type list
                   :initform '()
                   :documentation
                   "List of submodes objects that inherit from `pm-inner-submode'")
   (buffers :initarg :buffers
            :initform '()
            :type list
            :documentation
            "Holds all buffers associated with current buffer"))
   
   "Configuration for a polymode. Each polymode buffer contains a local
variable `pm/config' instantiated from this class or a subclass
of this class.")

(defclass pm-config-one (pm-config)
  ((inner-submode-name
     :initarg :inner-submode-name
     :type symbol
     :custom symbol
     :documentation
     "Symbol of the submode. At run time this object is cloned
     and placed in :inner-submodes slot."))
  
  "Configuration for a simple polymode that allows only one
submode. For example noweb.")


(defclass pm-config-multi (pm-config)
  ((inner-submode-names
     :initarg :inner-submode-names
     :type list
     :custom list
     :initform nil
     :documentation
     "List of names of the submode objects that are associated
     with this configuration. At initialization time, all of
     these are cloned and plased in :inner-submodes slot."))
  
  "Configuration for a polymode that allows multiple submodes
that are known in advance. For a variaty of web-modes.")



(defclass pm-config-multi-auto (pm-config-multi)
  ((auto-submode-name
    :initarg :auto-submode-name
    :type symbol
    :custom symbol
    :documentation
    "Symbol of auto submode. At run time this object is cloned
     and placed in :auto-submodes with coresponding :mode slot
     initialized at run time.")
   (auto-submodes
    :initarg :auto-submodes
    :type list
    :initform '()
    :documentation
    "List of submodes that are auto-generated in pm/get-span
    method for this class."))
  
  "Configuration for a polymode that allows multiple submode that
are not known in advance. For example org-mode, markdown.")



;;; SUBMODE
(defclass pm-submode (polymode)
  ((mode :initarg :mode
         :type symbol
         :initform 'fundamental-mode
         :custom symbol)
   (protect-indent-line-function :initarg :protect-indent-line-function
                                 :type boolean
                                 :initform nil
                                 :custom boolean
                                 :documentation
                                 "Whether to modify local
                                 `indent-line-function' by
                                 narrowing to current span
                                 first")
   (buffer :initarg :buffer
           :type (or null buffer)
           :initform nil))  
  "Represents a simple submode. Usually used for the definition of
  the base submodes (aka host submodes associated with the base
  buffer).")

(defclass pm-inner-submode (pm-submode)
  ((head-mode :initarg :head-mode
              :type symbol
              :initform 'fundamental-mode
              :custom symbol
              :documentation
              "Chunks' header mode. If set to 'body, the head is
              considered part of the chunk body. If set to 'base,
              head is considered part of the including base
              mode.")
   (head-buffer :initarg :head-buffer
                :type (or null buffer)
                :initform nil
                :documentation "This buffer is set automatically
                to :buffer if :head-mode is 'body, and to
                base-buffer if :head-mode is 'base")
   (tail-mode :initarg :tail-mode
              :type symbol
              :initform nil
              :custom symbol
              :documentation
              "If nil, it is the same as :HEAD-MODE. Otherwise,
              the same rules as for the :head-mode apply.")
   (tail-buffer :initarg :tail-buffer
                :type (or null buffer)
                :initform nil)
   (head-reg :initarg :head-reg
             :type (or string symbol)
             :initform ""
             :custom (or string symbol)
             :documentation "Regexp for the chunk start (aka head)")
   (tail-reg :initarg :tail-reg
             :type (or string symbol)
             :initform ""
             :custom (or string symbol)
             :documentation "Regexp for chunk end (aka tail)")
   (extensions :initarg :extensions
               :type list
               :custom list
               :documentation "List of file extensions the
               submode should be activated in.")
   (font-lock-keywords :initarg :font-lock-keywords
                       :type (or list symbol)
                       :initform nil)
   (font-lock-matcher :initarg :font-lock-matcher
                      :type (or list symbol)
                      :initform nil)
   (font-lock-syntactic-matcher :initarg :font-lock-syntactic-matcher
                                :type (or list symbol)
                                :initform nil)
   (font-lock-literal-matcher :initarg :font-lock-literal-matcher
                              :type (or list symbol)
                              :initform nil))
  "Representation of an inner (aka child) submode in a buffer.")

(defclass pm-inner-submode-auto (pm-inner-submode)
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
