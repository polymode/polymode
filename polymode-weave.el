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
    "Input-output specifications. An alist with elements of the form

          (ID REG-FROM EXT-TO DOC COMMMAND)

     ID is the unique identifier of the spec. REG-FROM is a
     regexp that is used to identify if current file can be
     weaved with this spec. EXT-TO is the *exact* (not regexp)
     extension of the output file. DOC is a short help string
     shown during interactive weaving. COMMMAND is the actual,
     weaver specific, command. It can contain the following
     format specs:
         %i - input file (no dir)
         %f - input file (full path) 
         %o - output file (no dir)
         %O - output file name (no dir, no extension)
         %p - output file (full path)")
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
                         (concat "weaving " from-to-id " with command:\n     " command "\n")))

(fset 'pm-default-shell-weave-sentinel (pm--make-shell-command-sentinel "weaving"))


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
        (pm--export-spec nil))
    (pm--weave-internal weaver fromto-id ifile cb)))

(defmethod pm-weave ((weaver pm-shell-weaver) fromto-id &optional ifile)
  (let ((cb (pm--wrap-callback weaver :sentinel ifile))
        (pm--export-spec nil))
    (pm--weave-internal weaver fromto-id ifile cb (oref weaver :quote))))

(defun pm--weave-internal (weaver from-to ifile &optional callback shell-quote)
  (flet ((squote (arg) (and arg (if shell-quote (shell-quote-argument arg) arg))))
    (let ((from-to-spec (assoc from-to (oref weaver :from-to))))
      (if from-to-spec
          (let* ((ifile (or ifile buffer-file-name))
                 (base-ofile (concat (format polymode-weave-output-file-format
                                             (file-name-base ifile))
                                     "." (nth 2 from-to-spec)))
                 (ofile (expand-file-name base-ofile (file-name-directory buffer-file-name)))
                 (command (format-spec (nth 4 from-to-spec)
                                       (list (cons ?i (squote (file-name-nondirectory ifile)))
                                             (cons ?f (squote ifile))
                                             (cons ?O (squote (file-name-base base-ofile)))
                                             (cons ?o (squote base-ofile))
                                             (cons ?p ofile)))))
            (message "Weaving '%s' with '%s' weaver ..."
                     (file-name-nondirectory ifile) (eieio-object-name weaver))
            ;; weave and pass to exporter if any
            (let* ((pm--output-file ofile)
                   (pm--input-file ifile)
                   (fun (oref weaver :function))
                   (wfile (if callback
                              (funcall fun command callback from-to)
                            (funcall fun command from-to))))
              ;; Display file when the worker returned a file.  Workers with
              ;; callbacks return nil and take care of display themselves.
              (when wfile
                (if pm--export-spec
                    ;; called by exporter
                    (pm-export (symbol-value (oref pm/polymode :exporter))
                               (car pm--export-spec) (cdr pm--export-spec) wfile)
                  (pm--display-file wfile)
                  wfile))))
        (error "from-to spec '%s' is not supported by weaver '%s'"
               from-to (eieio-object-name weaver))))))


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
