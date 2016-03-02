;;  -*- lexical-binding: t -*-
(require 'polymode-common)
(require 'polymode-classes)

(defgroup polymode-weave nil
  "Polymode Weavers"
  :group 'polymode)

(defcustom polymode-weave-output-file-format "%s[woven]"
  "Format of the weaved files.
%s is substituted with the current file name sans extension."
  :group 'polymode-weave
  :type 'string)

(defclass pm-weaver (pm-root)
  ((from-to
    :initarg :from-to
    :initform '()
    :type list
    :custom list
    :documentation
    "Input-output specifications. An alist with elements of the
    form (id reg-from ext-to doc command) or (id . selector).

     In both cases ID is the unique identifier of the spec. In
     the former case REG-FROM is a regexp used to identify if
     current file can be weaved with the spec. EXT-TO is the
     extension of the output file. DOC is a short help string
     used for interactive completion and messages. COMMAND is a
     weaver specific specific command. It can contain the
     following format specs:


         %i - input file (no dir)
         %I - input file (full path) 
         %o - output file (no dir)
         %O - output file (full path)
         %b - output file (base name only)
         %t - 4th element of the :to spec

     When specification is of the form (id . selector), SELECTOR
     is a function of variable arguments that accepts at least
     one argument ACTION. This function is called in a buffer
     visiting input file. ACTION is a symbol and can one of the
     following:

         match - must return non-nil if this specification
             applies to the file that current buffer is visiting,
             or :nomatch if specification does not apply.

         regexp - return a string which is used to match input
             file name. If nil, `match' selector must return
             non-nil value. This selector is ignored if `match'
             returned non-nil.

         output-file - return an output file name or a list of
           file names. Receives input-file as argument. If this
           command returns nil, the output is built from the
           input file name and value of 'output-ext command.

           This selector can also return a function. This
           function will be called in the callback or sentinel of
           the weaving process after the weaving was
           completed. This function should sniff the output of
           the process for errors or file names. It must return a
           file name, a list of file names or nil if no such
           files have been detected.

         ext - extension of output file. If nil and
           `output' also returned nil, the exporter won't be able
           to identify the output file and no automatic display
           or preview will be available.
     
         doc - return documentation string

         command - return a string to be used instead of
           the :from command. If nil, :from spec command is used.")
   (function
    :initarg :function
    :initform (lambda (command id)
                (error "No weaving function declared for this weaver"))
    :type (or symbol function)
    :documentation
    "Function to perform the weaving. Must take 2 arguments
     COMMAND and ID. COMMAND is the 5th argument of :from-to spec
     with all the formats substituted. ID is the id the
     corresponding element in :from-to spec.

     If this function returns a filename that file will be
     displayed to the user."))
  "Root weaver class.")

(defclass pm-callback-weaver (pm-weaver)
  ((callback
    :initarg :callback
    :initform (lambda (&optional rest)
                (error "No callback defined for this weaver."))
    :type (or symbol function)
    :documentation
    "Callback function to be called by :function. There is no
     default callback. Callbacks must return the output file."))
  "Class to represent weavers that call processes spanned by
  Emacs.")

(defclass pm-shell-weaver (pm-weaver)
  ((function
    :initform 'pm-default-shell-weave-function)
   (sentinel
    :initarg :sentinel
    :initform 'pm-default-shell-weave-sentinel
    :type (or symbol function)
    :documentation
    "Sentinel function to be called by :function when a shell
     call is involved. Sentinel must return the output file
     name.")
   (quote
    :initarg :quote
    :initform nil
    :type boolean
    :documentation "Non-nil when file arguments must be quoted
    with `shell-quote-argument'."))
  "Class for weavers that call external processes.")

(defun pm-default-shell-weave-function (command sentinel from-to-id &rest args)
  "Run weaving command interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts
user interaction. This is a default function in all weavers
that call a shell command"
  (pm--run-shell-command command sentinel "*polymode weave*"
                         (concat "weaving " from-to-id " with command:\n\n     "
                                 command "\n\n")))


;;; METHODS

(declare-function pm-export "polymode-export")

(defgeneric pm-weave (weaver from-to-id &optional ifile)
  "Weave current FILE with WEAVER.
WEAVER is an object of class `pm-weaver'. EXPORT is a list of the
form (FROM TO) suitable to be passed to `polymode-export'. If
EXPORT is provided, corresponding exporter's (from to)
specification will be called.")

(defmethod pm-weave ((weaver pm-weaver) from-to-id &optional ifile)
  (pm--weave-internal weaver from-to-id ifile))

(defmethod pm-weave ((weaver pm-callback-weaver) fromto-id &optional ifile)
  (let ((cb (pm--wrap-callback weaver :callback ifile))
        ;; with transitory output, callback might not run
        (pm--export-spec (and pm--output-not-real pm--export-spec)))
    (pm--process-internal weaver fromto-id nil ifile cb)))

(defmethod pm-weave ((weaver pm-shell-weaver) fromto-id &optional ifile)
  (let ((cb (pm--wrap-callback weaver :sentinel ifile))
        ;; with transitory output, callback might not run
        (pm--export-spec (and pm--output-not-real pm--export-spec)))
    (pm--process-internal weaver fromto-id nil ifile cb (oref weaver :quote))))


;; UI
(defvar pm--weaver-hist nil)
(defvar pm--weave:from-to-hist nil)

(defun polymode-weave (&optional from-to-id)
  "Weave current file.
First time this command is called in a buffer the user is asked
for the weaver to use from a list of known weavers.

Each weaver knows about at least one input-output
conversion. Appropriate input-output specification is set based
on this file's extension. If this detection is ambiguous ask the
user for weaver specification explicitly. If `from-to-id' is an
universal argument ask for specification regardless. If `from-to-id'
is a string, it is an ID of an entry in weaver's :from-to
input-output specification alist. See also `pm-weave' generic."
  (interactive "P")
  (let* ((weaver (symbol-value (or (oref pm/polymode :weaver)
                                   (polymode-set-weaver))))
         (w:fromto (oref weaver :from-to))
         (opts (mapcar (lambda (el)
                         (cons (format "%s" (nth 3 el)) (car el)))
                       w:fromto))
         (wname (eieio-object-name weaver))
         (ft-id
          (cond
           ;; guess from-to spec
           ((null from-to-id) (let ((fname (file-name-nondirectory buffer-file-name))
                                 (hist-from-to (pm--prop-get :weave-from-to))
                                 (case-fold-search t))
                             (or
                              ;; 1. repeated weave; don't ask and use first entry in history
                              (and hist-from-to (get-text-property 0 :id hist-from-to))
                              ;; 2. get first entry whose REG-FROM matches current file 
                              (car (cl-rassoc-if (lambda (el)
                                                   (string-match-p (car el) fname))
                                                 w:fromto))
                              ;; 3. nothing matched, ask
                              (let* ((prompt (format "No intpu-output spec for extension '.%s' in '%s' weaver. Choose one: "
                                                     (file-name-extension fname)
                                                     wname))
                                     (sel (completing-read prompt (mapcar #'car opts) nil t nil
                                                           'pm--weave:from-to-hist
                                                           hist-from-to)))
                                (pm--prop-put :weave-from-to sel)
                                (cdr (assoc sel opts))))))
           ;; C-u, force a :from-to spec
           ((equal from-to-id '(4)) (let ((sel (completing-read "Input type: " opts nil t nil
                                                             'pm--weave:from-to-hist
                                                             (pm--prop-get :weave-from-to)) ))
                                   (pm--prop-put :weave-from-to sel)
                                   (get-text-property 0 :id sel)))
           ;; string must match an entry
           ((stringp from-to-id) (if (assoc from-to-id w:fromto)
                                  from-to-id
                                (error "Cannot find input-output spec '%s' in %s weaver" from-to-id wname)))
           (t (error "'from-to-id' argument must be nil, universal argument or a string")))))
    (pm-weave weaver ft-id)))

(defmacro polymode-register-weaver (weaver defaultp &rest configs)
  "Add WEAVER to :weavers slot of all config objects in CONFIGS.
When DEFAULT? is non-nil, also make weaver the default WEAVER for
each polymode in CONFIGS."
  `(dolist (pm ',configs)
     (object-add-to-list (symbol-value pm) :weavers ',weaver)
     (when ,defaultp (oset (symbol-value pm) :weaver ',weaver))))

(defun polymode-set-weaver ()
  (interactive)
  (unless pm/polymode
    (error "No pm/polymode object found. Not in polymode buffer?"))
  (let* ((weavers (pm--abrev-names
                     (delete-dups (pm--oref-with-parents pm/polymode :weavers))
                     "pm-weaver/"))
         (sel (pm--completing-read "Choose weaver: " weavers nil t nil 'pm--weaver-hist))
         (out (intern (cdr sel))))
    (setq-local pm--weaver:from-last nil)
    (setq-local pm--weaver:to-last nil)
    (oset pm/polymode :weaver out)
    out))

(provide 'polymode-weave)
