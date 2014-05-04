(defgroup polymode-weave nil
  "Polymode Weavers"
  :group 'polymode)

(defcustom polymode-weave-output-file-format "%s[weaved]"
  "Format of the weaved files.
%s is substituted with the current file name sans extension."
  :group 'polymode-weave
  :type 'string)

(defclass pm-weaver (polymode)
  ((from-to
    :initarg :from-to
    :initform '()
    :type list
    :custom list
    :documentation
    "Input specifications. A list of lists of the form

          (id reg-from ext-to doc commmand)

     ID is the unique identifier of the spec. REG-FROM is a regexp
     that is used to identify if current file can be weaved with this
     spec. EXT-TO is the *exact* (not regexp) extension of the output
     file. DOC is a short help string shown during interactive
     weaving. COMMMAND is the actual weaver specific command. It can
     contain the following format specs:
	%i - replaced with the input file
	%o - replaced with the ouput file.
        %O - replaced with the base output file name (no dir, no extension)")
   (function
    :initarg :function
    :initform (lambda (command id)
                (error "No weaving function declared for this weaver"))
    :type (or symbol function)
    :documentation
    "Function to process the commmand. Must take 2 arguments
     COMMAND, ID. COMMAND is the 5th argument of :from spec with
     all the formats substituted. ID is the id of requested :from
     spec."))
  "Root weaver class.")

(defclass pm-callback-weaver (pm-weaver)
  ((callback
    :initarg :callback
    :initform (lambda (&optional rest)
                (error "No callback defined for this weaver."))
    :type (or symbol function)
    :documentation
    "Callback function to be called by :function when a shell
    call is involved. There is no default callback."))
  "Class to represent weavers that call processes spanned by
  emacs. Callback should return the output file name.")

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
    name."))
  "Class to represent weavers that call external processes.")


;;; METHODS
(defgeneric pm-weave (weaver from-to &optional export ifile)
  "Weave current FILE with WEAVER.
EXPORT must be a list of the form (FROM TO) sutable for call of
`polymode-export'. If EXPORT is provided corresponding
exporter (from to) specification will be called.")

(defmethod pm-weave ((weaver pm-weaver) from-to &optional export ifile)
  (let ((from-to-spec (assoc from-to (oref weaver :from-to))))
    (if from-to-spec
        (let* ((ofile (concat (format polymode-weave-output-file-format
                                      (file-name-base buffer-file-name))
                              "." (nth 2 from-to-spec)))
               (ifile (or ifile
                          (file-name-nondirectory buffer-file-name)))
               (command (format-spec (nth 4 from-to-spec)
                                     (list (cons ?i ifile)
                                           (cons ?O (file-name-base ofile))
                                           (cons ?o ofile)))))
          ;; compunicate with sentinel and callback with local vars in order to
          ;; avoid needless clutter
          (set (make-local-variable 'pm--output-file) ofile)
          (set (make-local-variable 'pm--input-file) ifile)
          (message "Weaving '%s' with '%s' weaver ..."
                   (file-name-nondirectory ifile) (pm--object-name weaver))
          (let ((wfile (funcall (oref weaver :function) command from-to)))
            (when wfile 
              (if export    
                  (pm-export (symbol-value (oref pm/config :exporter))
                             (car export) (cdr export) wfile)
                ;; display the file only when the worker returns the
                ;; file. Sentinel and callback based weavers return nil.
                (pm--display-file wfile)
                wfile))))
      (error "from-to spec '%s' is not supported by weaver '%s'"
             from-to (pm--object-name weaver)))))

(defun pm--display-file (ofile)
  (display-buffer (find-file-noselect ofile 'nowarn)))

(defmacro pm--weave-wrap-callback (slot)
  ;; replace weaver :sentinel or :callback temporally in order to export as a
  ;; followup step or display the result
  `(let ((sentinel1 (oref weaver ,slot)))
    (condition-case err
        (let ((sentinel2 (if export
                             `(lambda (proc name)
                                (let ((wfile (,sentinel1 proc name)))
                                  ;;; we don't return file here, kinda okward
                                  (pm-export (symbol-value ',(oref pm/config :exporter))
                                             ,(car export) ,(cdr export)
                                             wfile)))
                           `(lambda (proc name)
                              (let ((wfile (,sentinel1 proc name)))
                                (pm--display-file wfile)
                                wfile)))))
          (oset weaver ,slot sentinel2)
          ;; don't pass EXPORT argument, it is called from sentinel 
          (call-next-method weaver from-to nil ifile))
      (error (oset weaver ,slot sentinel1)
             (signal (car err) (cdr err))))
    (oset weaver ,slot sentinel1)))

(defmethod pm-weave ((weaver pm-shell-weaver) from-to &optional export ifile)
  (pm--weave-wrap-callback :sentinel))

(defmethod pm-weave ((weaver pm-callback-weaver) from-to &optional export ifile)
  (pm--weave-wrap-callback :callback))


;; UI
(defvar pm--weaver-hist nil)
(defvar pm--weave:from-to-hist nil)

(defun polymode-weave (&optional from-to)
  "todo:
See `pm-weave' generic.
FROM-TO ignored as yet"
  (interactive "P")
  (let* ((weaver (symbol-value (or (oref pm/config :weaver)
                                   (polymode-set-weaver))))
         (w:from-to (oref weaver :from-to))
         (opts (mapcar (lambda (el)
                         (propertize (format "%s" (nth 3 el)) :id (car el)))
                       w:from-to))
         (wname (pm--object-name weaver))
         (from-to
          (cond ((null from-to)
                 (let ((fname (file-name-nondirectory buffer-file-name))
                       (hist-from-to (pm--get-hist :weave-from-to))
                       (case-fold-search t))
                   (or (and hist-from-to
                            (get-text-property 0 :id  hist-from-to))
                       (car (cl-rassoc-if (lambda (el)
                                            ;; (dbg (car el) fname)
                                            (string-match-p (car el) fname))
                                          w:from-to))
                       (let* ((prompt (format "No intpu-output spec for weaver '%s'. Choose one: "
                                              (file-name-extension fname)
                                              wname))
                              (sel (ido-completing-read prompt opts nil t nil
                                                        'pm--weave:from-to-hist
                                                        (pm--get-hist :weave-from-to))))
                         (pm--put-hist :weave-from-to sel)
                         (get-text-property 0 :id sel)))))
                ;; C-u, force a :from-to spec
                ((equal from-to '(4))
                 (let ((sel (ido-completing-read "Input type: " opts nil t nil
                                                 'pm--weave:from-to-hist
                                                 (pm--get-hist :weave-from-to)) ))
                   (pm--put-hist :weave-from-to sel)
                   (get-text-property 0 :id sel)))
                ((stringp from-to)
                 (if (assoc from-to w:from-to)
                     from-to
                   (error "Cannot find input-output spec '%s' in %s weaver" from-to wname)))
                (t (error "'from-to' argument must be nil, universal argument or a string")))))
    (pm-weave weaver from-to)))

(defmacro polymode-register-weaver (weaver default? &rest configs)
  "Add WEAVER to :weavers slot of all config objects in CONFIGS.
When DEFAULT? is non-nil, also make weaver the default WEAVER for
each polymode in CONFIGS."
  `(dolist (pm ',configs)
     (object-add-to-list (symbol-value pm) :weavers ',weaver)
     (when ,default? (oset (symbol-value pm) :weaver ',weaver))))


(defun polymode-set-weaver ()
  (interactive)
  (unless pm/config
    (error "No pm/config object found. Not in polymode buffer?"))
  (let* ((weavers (pm--abrev-names
                     (delete-dups (pm--oref-with-parents pm/config :weavers))
                     "pm-weaver/"))
         (sel (ido-completing-read "No default weaver. Choose one: " weavers nil t nil
                                   'pm--weaver-hist (car pm--weaver-hist)))
         (out (intern (get-text-property 0 :orig sel))))
    (oset pm/config :weaver out)
    out))



;; UTILS
(defun pm-default-shell-weave-sentinel (process name)
  "Default weaver sentinel."
  (pm--run-command-sentinel process name "weaving"))

(defun pm-default-shell-weave-function (command from-to)
  "Run weaving command interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts
user interaction. This is a default function in all weavers
that call a shell command"
  (pm--run-command command
                   (oref (symbol-value (oref pm/config :weaver))
                         :sentinel)  
                   "*polymode weave*"
                   (concat "weaving " from-to " with command:\n     "
                           command "\n")))
