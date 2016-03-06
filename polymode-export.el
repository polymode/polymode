(require 'polymode-core)
(require 'polymode-classes)

(defgroup polymode-export nil
  "Polymode Exporters"
  :group 'polymode)

(defcustom polymode-exporter-output-file-format "%s[exported]"
  "Format of the exported files.
%s is substituted with the current file name sans extension."
  :group 'polymode-export
  :type 'string)

(defclass pm-exporter (pm-root)
  ((from
    :initarg :from
    :initform '()
    :type list
    :custom list
    :documentation
    "Input exporter specifications.
     This is an alist of elements of the form (id regexp doc
     commmand) or (id . selector). ID is the unique identifier of
     the spec. REGEXP is a regexp which, if matched on current
     file name, implies that the current file can be exported
     with this specification. DOC is a short help string shown
     during interactive export. COMMAND is the exporter
     command (string). It can contain the following format specs:

         %i - input file (no dir)
         %I - input file (full path)
         %o - output file (no dir)
         %O - output file (full path)
         %b - output file (base name only)
         %t - 4th element of the :to spec

     When specification is of the form (id . selector), SELECTOR
     is a function of variable arguments that accepts at least
     one argument ACTION. ACTION is a symbol and can be one of
     the following:

         match - must return non-nil if this specification
             applies to the file that current buffer is visiting,
             or :nomatch if specification does not apply. This
             selector can receive an optional file-name
             argument. In that case the decision must be made
             solely on that file and current buffer must be
             ignored. This is useful for matching exporters to
             weavers when exported file does not exist yet.

         regexp - return a string which is used to match input
             file name. If nil, `match' selector must return
             non-nil value. This selector is ignored if `match'
             returned non-nil.

         doc - return documentation string

         commmand - return a string with optional %i, %f,
             etc. format specs as described above. It will be
             passed to the processing :function.")

   (to
    :initarg :to
    :initform '()
    :type list
    :custom list
    :documentation
    "Output specifications alist. Each element is either a list
     of the form (id ext doc t-spec) or a cons (id . selector).

     In the former case EXT is an extension of the output
     file. DOC is a short documentation string. t-spec is a
     string what is substituted instead of %t in :from spec
     commmand. `t-spec' can be a list of one element '(command),
     in which case the whole :from spec command is substituted
     with command from %t-spec.

     When specification is of the form (id . selector), SELECTOR
     is a function of variable arguments that accepts at least
     one argument ACTION. This function is called in a buffer
     visiting input file. ACTION is a symbol and can one of the
     following:

         output-file - return an output file name or a list of file
           names. Receives input-file as argument. If this
           command returns nil, the output is built from input
           file and value of 'output-ext command.


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
           the :from command. If nil, :from spec command is used.

         t-spec - return a string to be substituted as %t :from
           spec in :from command. If `command' selector returned
           non-nil, this spec is ignored.")
   (function
    :initarg :function
    :initform (lambda (command from to)
                (error "Function not defined for this exporter"))
    :type (or symbol function)
    :documentation
    "Function to process the commmand. Must take 3 arguments
     COMMAND, FROM-ID and TO-ID. COMMAND is the 4th argument
     of :from spec with all the formats substituted. FROM-ID is
     the id of requested :from spec, TO-ID is the id of the :to
     spec."))
  "Root exporter class.")

(defclass pm-callback-exporter (pm-exporter)
  ((callback
    :initarg :callback
    :initform (lambda (&optional rest)
                (error "No callback defined for this exporter."))
    :type (or symbol function)
    :documentation
    "Callback function to be called by :function. There is no
     default callback. Callback must return the output file
     name."))
  "Class to represent asynchronous exporters.")

(defclass pm-shell-exporter (pm-exporter)
  ((function
    :initform 'pm-default-shell-export-function)
   (sentinel
    :initarg :sentinel
    :initform 'pm-default-export-sentinel
    :type (or symbol function)
    :documentation
    "Sentinel function to be called by :function when a shell
    call is involved. Sentinel should return the output file
    name.")
   (quote
    :initarg :quote
    :initform nil
    :type boolean
    :documentation "Non-nil when file arguments must be quoted
    with `shell-quote-argument'."))
  "Class to represent exporters that call external processes.")

(defun pm-default-shell-export-function (command sentinel from to)
  "Run exporting command interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts
user interaction. This is a default function in all exporters
that call a shell command"
  (pm--run-shell-command command sentinel "*polymode export*"
                         (concat "Exporting " from "-->" to " with command:\n\n     "
                                 command "\n\n")))


;;; METHODS

(defgeneric pm-export (exporter from to &optional ifile)
  "Process IFILE with EXPORTER.")

(defmethod pm-export ((exporter pm-exporter) from to &optional ifile)
  (pm--process-internal exporter from to ifile))

(defmethod pm-export ((exporter pm-callback-exporter) from to &optional ifile)
  (let ((cb (pm--wrap-callback exporter :callback ifile)))
    (pm--process-internal exporter from to ifile cb)))

(defmethod pm-export ((exporter pm-shell-exporter) from to &optional ifile)
  (let ((cb (pm--wrap-callback exporter :sentinel ifile)))
    (pm--process-internal exporter from to ifile cb (oref exporter :quote))))


;; UI

(defvar pm--exporter-hist nil)
(defvar pm--export:from-hist nil)
(defvar pm--export:from-last nil)
(defvar pm--export:to-hist nil)
(defvar pm--export:to-last nil)
(declare-function polymode-set-weaver "polymode-weave")
(declare-function pm-weave "polymode-weave")

(defun polymode-export (&optional from to)
  "Export current file.

FROM and TO are the ids of the :from and :to slots of the current
exporter. If the current exporter hasn't been set yet, set the
exporter with `polymode-set-exporter'. You can always change the
exporter manually by invoking `polymode-set-exporter'.

When FROM or TO are missing they are determined automatically
from the current exporter's specifications and file's
extension. If no appropriate export specification has been found,
look into current weaver and try to match weaver's output to
exporters input extension. When such combination is possible,
settle on weaving first and exporting the weaved output. When
none of the above worked, ask the user for `from' and `to' specs.

When called interactively with C-u argument, ask for FROM and TO
interactively. See class `pm-exporter' for the complete
specification."
  (interactive "P")
  (cl-flet ((to-name.id (el) (let* ((ext (funcall (cdr el) 'ext))
                                    (name (if ext
                                              (format "%s (%s)" (funcall (cdr el) 'doc) ext)
                                            (funcall (cdr el) 'doc))))
                               (cons name (car el))))
            (from-name.id (el) (cons (funcall (cdr el) 'doc) (car el))))
    (let* ((exporter (symbol-value (or (oref pm/polymode :exporter)
                                       (polymode-set-exporter))))
           (fname (file-name-nondirectory buffer-file-name))
           (gprompt nil)
           (case-fold-search t)

           (from-opts (mapcar #'from-name.id (pm--selectors exporter :from)))
           (from-id
            (cond
             ;; A: guess from spec
             ((null from)
              (or
               ;; 1. repeated export; don't ask
               pm--export:from-last

               ;; 2. select :from entries which match to current file
               (let ((matched (cl-loop for el in (pm--selectors exporter :from)
                                       when (pm--selector-match (cdr el))
                                       collect (from-name.id el))))
                 (when matched
                   (if (> (length matched) 1)
                       (cdr (pm--completing-read "Multiple `from' specs matched. Choose one: " matched))
                     (cdar matched))))

               ;; 3. guess from weaver and return a cons (weaver-id . exporter-id)
               (let ((weaver (symbol-value (or (oref pm/polymode :weaver)
                                               (progn
                                                 (setq gprompt "Choose `from' spec: ")
                                                 (when (y-or-n-p "No `from' specs matched. Set weaver?")
                                                   (polymode-set-weaver)))))))
                 (when weaver
                   ;; fixme: weaver was not yet ported to selectors
                   ;; fixme: currently only first match is returned
                   (let ((pair (cl-loop for w in (oref weaver :from-to)
                                        ;; weaver input extension matches the filename
                                        if (string-match-p (nth 1 w) fname)
                                        return (cl-loop for el in (pm--selectors exporter :from)
                                                        ;; input exporter extensnion matches weaver output extension
                                                        when (pm--selector-match (cdr el) (concat "dummy." (nth 2 w)))
                                                        return (cons (car w) (car el))))))
                     (when pair
                       (message "Matching weaver found. Weaving to '%s' first." (car pair))
                       pair))))

               ;; 4. nothing matched; ask
               (let* ((prompt (or gprompt
                                  (format "No `from' specs matched. Choose one: "
                                          (file-name-nondirectory fname) (eieio-object-name-string exporter))))
                      (sel (pm--completing-read prompt from-opts nil t nil 'pm--export:from-hist)))
                 (cdr sel))))

             ;; B: C-u, force a :from spec
             ((equal from '(4))
              (cdr (if (> (length from-opts) 1)
                       (pm--completing-read "Input type: " from-opts nil t nil 'pm--export:from-hist)
                     (car from-opts))))

             ;; C. string
             ((stringp from)
              (if (assoc from (oref exporter :from))
                  from
                (error "Cannot find `from' spec '%s' in %s exporter"
                       from (eieio-object-name exporter))))
             ;; D. error
             (t (error "'from' argument must be nil, universal argument or a string"))))

           (to-opts (mapcar #'to-name.id (pm--selectors exporter :to)))
           (to-id
            (cond
             ;; A. guess from spec
             ((null to)
              (or
               ;; 1. repeated export; don't ask and use first entry in history
               (unless (equal from '(4))
                 pm--export:to-last)

               ;; 2. First export or C-u
               (cdr (pm--completing-read "Export to: " to-opts nil t nil 'pm--export:to-hist))))

             ;; B. string
             ((stringp to)
              (if (assoc to (oref exporter :to))
                  to
                (error "Cannot find output spec '%s' in %s exporter"
                       to (eieio-object-name exporter))))
             ;; C . Error
             (t (error "'to' argument must be nil or a string")))))

      (setq-local pm--export:from-last from-id)
      (setq-local pm--export:to-last to-id)

      (if (consp from-id)
          ;; run through weaver
          (let ((pm--export-spec (cons (cdr from-id) to-id))
                (pm--output-not-real t))
            (pm-weave (symbol-value (oref pm/polymode :weaver)) (car from-id)))
        (pm-export exporter from-id to-id)))))

(defun polymode-set-exporter ()
  "Interactively set exporter for the current file."
  (interactive)
  (unless pm/polymode
    (error "No pm/polymode object found. Not in polymode buffer?"))
  (let* ((exporters (pm--abrev-names
                     (delete-dups (pm--oref-with-parents pm/polymode :exporters))
                     "pm-exporter/"))
         (sel (pm--completing-read "Choose exporter: " exporters nil t nil 'pm--exporter-hist))
         (out (intern (cdr sel))))
    (setq-local pm--export:from-last nil)
    (setq-local pm--export:to-last nil)
    (oset pm/polymode :exporter out)
    out))

(defmacro polymode-register-exporter (exporter defaultp &rest configs)
  "Add EXPORTER to :exporters slot of all config objects in CONFIGS.
When DEFAULT? is non-nil, also make EXPORTER the default exporter
for each polymode in CONFIGS."
  `(dolist (pm ',configs)
     (object-add-to-list (symbol-value pm) :exporters ',exporter)
     (when ,defaultp (oset (symbol-value pm) :exporter ',exporter))))


;;; GLOBAL EXPORTERS
(defcustom pm-exporter/pandoc
  (pm-shell-exporter "pandoc"
               :from
               '(;; ("json" "\\.json\\'" "JSON native AST"  "pandoc %i -f json -t %t -o %o")
                 ("markdown"    "\\.md\\'" "pandoc's markdown"  "pandoc %i -f markdown -t %t -o %o")
                 ("markdown_strict" "\\.md\\'" "original markdown"  "pandoc %i -f markdown_strict -t %t -o %o")
                 ("markdown_phpextra"   "\\.md\\'" "PHP markdown"   "pandoc %i -f markdown_phpextra -t %t -o %o")
                 ("markdown_phpextra"   "\\.md\\'" "github markdown"    "pandoc %i -f markdown_phpextra -t %t -o %o")
                 ("textile" "\\.textile\\'" "Textile"       "pandoc %i -f textile -t %t -o %o")
                 ("rst"     "\\.rst\\'" "reStructuredText"  "pandoc %i -f rst -t %t -o %o")
                 ("html"    "\\.x?html?\\'" "HTML"  "pandoc %i -f html -t %t -o %o")
                 ("doocbook"    "\\.xml\\'" "DocBook"       "pandoc %i -f doocbook -t %t -o %o")
                 ("mediawiki"   "\\.wiki\\'" "MediaWiki"        "pandoc %i -f mediawiki -t %t -o %o")
                 ("latex"   "\\.tex\\'" "LaTeX"         "pandoc %i -f latex -t %t -o %o")
                 )
               :to
               '(;; ("json"     "json"  "JSON version of native AST" "json")
                 ("plain"   "txt"  "plain text" "plain")
                 ("markdown"    "md"  "pandoc's extended markdown" "markdown")
                 ("markdown_strict"     "md"  "original markdown" "markdown_strict")
                 ("markdown_phpextra"   "md"  "PHP extended markdown" "markdown_phpextra")
                 ("markdown_github"     "md"  "github extended markdown" "markdown_github")
                 ("rst"     "rst"  "reStructuredText" "rst")
                 ("html"    "html"  "XHTML 1" "html")
                 ("html5"   "html"  "HTML 5" "html5")
                 ("latex"   "tex"  "LaTeX" "latex")
                 ("beamer"      "tex"  "LaTeX beamer" "beamer")
                 ("context"     "tex"  "ConTeXt" "context")
                 ("man"     "man"  "groff man" "man")
                 ("mediawiki"   "wiki"  "MediaWiki markup" "mediawiki")
                 ("textile"     "textile"  "Textile" "textile")
                 ("org"     "org"  "Emacs Org-Mode" "org")
                 ("texinfo"     "info"  "GNU Texinfo" "texinfo")
                 ("docbook"     "xml"  "DocBook XML" "docbook")
                 ("opendocument"    "xml"  "OpenDocument XML" "opendocument")
                 ("odt"     "odt"  "OpenOffice text document" "odt")
                 ("docx"    "docx"  "Word docx" "docx")
                 ("epub"    "epub"  "EPUB book" "epub")
                 ("epub3"   "epub"  "EPUB v3" "epub3")
                 ("fb2"     "fb"  "FictionBook2 e-book" "fb2")
                 ("asciidoc"    "txt"  "AsciiDoc" "asciidoc")
                 ("slidy"   "html"  "Slidy HTML slide show" "slidy")
                 ("slideous"    "html"  "Slideous HTML slide show" "slideous")
                 ("dzslides"    "html"  "HTML5 slide show" "dzslides")
                 ("s5"      "html"  "S5 HTML slide show" "s5")
                 ("rtf"     "rtf"  "rich text format" "rtf"))
               :function 'pm-default-shell-export-function
               :sentinel 'pm-default-export-sentinel)
  "Pandoc exporter"
  :group 'polymode-export
  :type 'object)

(provide 'polymode-export)
