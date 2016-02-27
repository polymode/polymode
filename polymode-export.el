(require 'polymode-common)
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
    "Input specifications. An alist elements of the form (id reg doc commmand).
     ID is the unique identifier of the spec. REG is a regexp
     that is used to identify if current file can be exported
     with this spec. DOC is a short help string shown during
     interactive export. COMMMAND is the actual exporter specific
     command. It can contain the following format specs:

         %i - input file (no dir)
         %f - input file (full path) 
         %o - output file (no dir)
         %O - output file name (no dir, no extension)
         %p - output file (full path)
         %t - 4th element of the :to spec")
   (to
    :initarg :to
    :initform '() 
    :type list
    :custom list
    :documentation
    "Output specifications. An alist of elements of the form (id ext doc %t-spec).
     EXT is the *exact* extension (not a regexp) of the ouput
     file. %t-spec is a string what is used to format :from spec
     commmand.")
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
    name."))
  "Class to represent exporters that call external processes.")

(defun pm-default-shell-export-function (command sentinel from to)
  "Run exporting command interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts
user interaction. This is a default function in all exporters
that call a shell command"
  (pm--run-shell-command command sentinel "*polymode export*"
                         (concat "Exporting " from "-->" to " with command:\n     " command "\n")))

(fset 'pm-default-export-sentinel (pm--make-shell-command-sentinel "error" "exporting"))


;;; METHODS

(defgeneric pm-export (exporter from to &optional ifile)
  "Process IFILE with EXPORTER.")

(defmethod pm-export ((exporter pm-exporter) from to &optional ifile)
  (pm--export-internal exporter from to ifile))

(defmethod pm-export ((exporter pm-callback-exporter) from to &optional ifile)
  (let ((cb (pm--wrap-callback exporter :callback ifile)))
    (pm--export-internal exporter from to ifile cb)))

(defmethod pm-export ((exporter pm-shell-exporter) from to &optional ifile)
  (let ((cb (pm--wrap-callback exporter :sentinel ifile)))
    (pm--export-internal exporter from to ifile cb)))

(defun pm--export-internal (exporter from to ifile &optional callback)
  (unless (and from to)
    (error "Both FROM and TO must be supplied (from: %s, to: %s)" from to))
  (let* ((from-spec (assoc from (oref exporter :from)))
         (to-spec (assoc to (oref exporter :to)))
         (ifile (or ifile buffer-file-name))
         (base-ofile (concat (format polymode-exporter-output-file-format
                                     (file-name-base buffer-file-name))
                             "." (nth 1 to-spec)))
         (ofile (expand-file-name base-ofile (file-name-directory buffer-file-name)))
         (command (format-spec (nth 3 from-spec)
                               (list (cons ?i (file-name-nondirectory ifile))
                                     (cons ?f ifile)
                                     (cons ?O (file-name-base base-ofile))
                                     (cons ?o base-ofile)
                                     (cons ?p ofile)
                                     (cons ?t (nth 3 to-spec))))))
    (unless to-spec
      (error "'to' spec `%s' is not defined for exporter '%s'" to (pm--object-name exporter)))
    (message "Exporting '%s' with '%s' exporter ..."
             (file-name-nondirectory ifile) (pm--object-name exporter))
    (let* ((pm--output-file ofile)
           (pm--input-file ifile)
           (fun (oref exporter :function))
           (efile (if callback
                      (funcall fun command callback from to)
                    (funcall fun command from to))))
      (and efile (pm--display-file ofile)))))


;; UI

(defvar pm--exporter-hist nil)
(defvar pm--export:from-hist nil)
(defvar pm--export:to-hist nil)
(declare-function polymode-set-weaver "polymode-weave")
(declare-function pm-weave "polymode-weave")

(defun polymode-export (&optional from to)
  "Export current file.

FROM and TO are the :from-id and :to-id in the definition of the
current exporter. If current exporter hasn't been set yet, call
`polymode-set-exporter' before exporting.

If called interactively with C-u argument, ask for FROM
interactively, otherwise FROM and TO are determined automatically
from the current exporter specification and current file
extension.  See class `pm-exporter' for the definitions."
  (interactive "P")
  (let* ((exporter (symbol-value (or (oref pm/polymode :exporter)
                                     (polymode-set-exporter))))
         (fname (file-name-nondirectory buffer-file-name))
         (e:from (oref exporter :from))
         (e:to (oref exporter :to))
         (from-opts (mapcar (lambda (el)
                              (cons (nth 2 el) (car el)))
                            (oref exporter :from)))
         (to-opts (mapcar (lambda (el)
                            (cons (format "%s (%s)" (nth 2 el) (nth 1 el)) (car el)))
                          (oref exporter :to)))
         (from
          (cond ((null from)
                 ;; A: guess from spec
                 (let ((case-fold-search  t))
                   (or
                    ;; 1. repeated export; don't ask and use first entry in history
                    (and (pm--get-hist :export-from)
                         (get-text-property 0 :id (pm--get-hist :export-from)))
                    ;; 2. get first entry whose REG matches file name
                    (car (cl-rassoc-if (lambda (el)
                                         (string-match-p (car el) fname))
                                       e:from))
                    ;; 3. guess from weaver and return a cons (weaver-id . exporter-id)
                    (let ((weaver (symbol-value (or (oref pm/polymode :weaver)
                                                    (polymode-set-weaver)))))
                      (cl-loop for w in (oref weaver :from-to)
                               ;; weaver intput extension matches the filename
                               if (string-match-p (nth 1 w) fname) 
                               return (cl-loop for e in e:from
                                               ;; input exporter extensnion matches weaver output extension
                                               if (string-match-p (nth 1 e) (concat "dummy." (nth 2 w)))
                                               return (cons (car w) (car e)))))
                    ;; 4. nothing matched; ask
                    (let* ((prompt (format "No input spec for extension '.%s' in '%s' exporter. Choose one: "
                                           (file-name-extension fname)
                                           (pm--object-name exporter)))
                           (sel (completing-read prompt (mapcar #' car from-opts) nil t nil
                                                 'pm--export:from-hist (pm--get-hist :export-from))))
                      (pm--put-hist :export-from sel)
                      (cdr (assoc sel from-opts))))))
                ;; B: C-u, force a :from spec
                ((equal from '(4))
                 (let ((sel (completing-read "Input type: " (mapcar #' car from-opts) nil t nil
                                             'pm--export:from-hist (pm--get-hist :export-from)) ))
                   (pm--put-hist :export-from sel)
                   (cdr (assoc sel from-opts))))
                ;; C. string
                ((stringp from)
                 (if (assoc from e:from)
                     from
                   (error "Cannot find input spec '%s' in %s exporter"
                          from (pm--object-name exporter))))
                (t (error "'from' argument must be nil, universal argument or a string"))))
         (to
          (cond ((null to)
                 (let ((sel (completing-read "Export to: " (mapcar #'car to-opts) nil t nil
                                             'pm--export:to-hist (pm--get-hist :export-to))))
                   (pm--put-hist :export-to sel)
                   (cdr (assoc sel to-opts))))
                ((stringp to)
                 (if (assoc to e:to)
                     to
                   (error "Cannot find output spec '%s' in %s exporter"
                          to (pm--object-name exporter))))
                (t (error "'to' argument must be nil or a string")))))
    (if (consp from)
        ;; run through weaver
        (let ((pm--export-spec (cons (cdr from) to)))
          (pm-weave (symbol-value (oref pm/polymode :weaver)) (car from)))
      (pm-export exporter from to))))

(defun polymode-set-exporter ()
  (interactive)
  (unless pm/polymode
    (error "No pm/polymode object found. Not in polymode buffer?"))
  (let* ((exporters (pm--abrev-names
                     (delete-dups (pm--oref-with-parents pm/polymode :exporters))
                     "pm-exporter/"))
         (sel (completing-read "Choose exporter: " (mapcar #'car exporters) nil t nil
                               'pm--exporter-hist (car pm--exporter-hist)))
         (out (intern (cdr (assoc sel exporters)))))
    (oset pm/polymode :exporter out)
    out))

(defmacro polymode-register-exporter (exporter default? &rest configs)
  "Add EXPORTER to :exporters slot of all config objects in CONFIGS.
When DEFAULT? is non-nil, also make EXPORTER the default exporter
for each polymode in CONFIGS."
  `(dolist (pm ',configs)
     (object-add-to-list (symbol-value pm) :exporters ',exporter)
     (when ,default? (oset (symbol-value pm) :exporter ',exporter))))


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
                 ("rtf"     "rtf"  "rich text format" "rtf")
                 )
               :function 'pm-default-shell-export-function
               :sentinel 'pm-default-export-sentinel)
  "Pandoc exporter"
  :group 'polymode-export
  :type 'object)

(provide 'polymode-export)
