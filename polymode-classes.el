;;; ROOT CLASS
(defclass polymode (eieio-instance-inheritor) ()
  "Root polymode class.")

;;; CONFIG
(defclass pm-config (polymode) 
  ((base-mode :initarg :base-mode
              :initform 'fundamental-mode
              :type symbol
              :custom symbol
              :documentation "Mode of the base buffer - the host mode.")
   (submodes :initarg :submodes
             :type list
             :documentation
             "List of submodes objects that inherit from `pm-submode'"))
   
   "Configuration for a polymode. Each polymode buffer contains a local
variable `pm/config' instantiated from this class or a subclass
of this class.")

(defclass pm-config-simple (pm-config)
  ((default-submode
     :initarg :default-submode
     :type symbol
     :custom symbol
     :documentation"Symbol of the submode that is activated by default"))
  "Configuration for a simple polymode that allows only one
submode. For example noweb.")


(defclass pm-config-multi (pm-config)
  ((default-submodes
     :initarg :default-submodes
     :type list
     :custom list
     :documentation "Symbol of the submode that is activated by default"))
  
  "Configuration for a simple polymode that allows only one
submode. For example org-mode, markdown or variety of
web-modes.")



;;; SUBMODE
(defclass pm-submode (polymode)
  ((header-reg :initarg :header-reg
              :type string
              :initform ""
              :custom string
              :documentation "Regexp for chunk start")
   (tail-reg :initarg :tail-reg
            :type string
            :initform ""
            :custom string
            :documentation "Regexp for chunk end.")
   ;; (header-include :initarg :header-include
   ;;                 :type boolean
   ;;                 :initform t
   ;;                 :custom boolean
   ;;                 :documentation "t if region matched by START-REG is part of the chunk")
   ;; (tail-include :initarg :tail-include
   ;;              :type boolean
   ;;              :initform t
   ;;              :custom boolean
   ;;              :documentation "t if region matched by END-REG is part of the chunk")
   (extensions :initarg :extensions
               :type list
               :custom list
               :documentation "List of file extensions the submode should be activated in.")
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
                              :initform nil)
   )
  (:abstract t)
  "Abstract class to represent a submode.")


(defclass pm-submode-simple (pm-submode)
  ((mode :initarg :mode
         :type symbol
         :initform 'fundamental-mode
         :custom symbol)
   (buffer :initarg :buffer
           :type (or null buffer)
           :initform nil) 
   (header-mode :initarg :header-mode
                :type symbol
                :initform 'fundamental-mode
                :custom symbol
                :documentation "Chunks' header mode. If set to
                'chunk', header is considered part of the
                chunk. If set to 'base', header is considered
                part of the including base mode.")
   (header-buffer :initarg :buffer
           :type (or null buffer)
           :initform nil)
   (tail-mode :initarg :tail-mode
              :type symbol
              :initform nil
              :custom symbol
              :documentation
              "If nil, it is the same as :HEADER-MODE. Otherwise,
              the same rules as for header-mode apply.")
   (tail-buffer :initarg :buffer
           :type (or null buffer)
           :initform nil))
  
  "Manage one major mode and one submode with its indirect
  buffer. This is most common submode.")

