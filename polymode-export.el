(defgroup polymode-export nil
  "Polymode Exporters"
  :group 'polymode)

(defcustom polymode-exporter-output-file-format "%s[exported]"
  "Format of the exported files.
%s is substituted with the current file name sans extension."
  :group 'polymode-export
  :type 'string)

(defclass pm-exporter (polymode)
  ((from
    :initarg :from
    :initform '()
    :type list
    :custom list
    :documentation
    "Input specifications. A list of lists of the form (id reg doc commmand).
	ID is the unique identifier of the spec. REG is a regexp
	that is used to identify if current file can be exported
	with this spec. DOC is a short help string shown during
	interactive export. COMMMAND is the actual exporter
	specific command. It can contain the following format
	specs:

	%i - replaced with the input file
	%o - replaced with the ouput file
        %O - replaced with the base output file name (no dir, no extension)
	%t - replaced with the 4th element of the :to spec.
     ")
   (to
    :initarg :to
    :initform '() 
    :type list
    :custom list
    :documentation
    "Output specifications. A list of the list of the form (id ext doc %t-spec).
	EXT is the *exact* extension (not a regexp) of the ouput
	file. %t-spec is a string what is used to format :from
	spec commmand.")
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
    "Callback function to be called by :function when a shell
    call is involved. There is no default callback."))
  "Class to represent exporters that call processes spanned by
  emacs. Callback should return the output file name.")

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


;;; METHODS
(defgeneric pm-export (exporter from to &optional ifile)
  "Weave current FILE with EXPORTER.")

(defmethod pm-export ((exporter pm-exporter) from to &optional ifile)
  (let* ((from-spec (assoc from (oref exporter :from)))
         (to-spec (assoc to (oref exporter :to)))
         (ifile (or ifile (file-name-nondirectory buffer-file-name)))
         (ofile (concat (format polymode-exporter-output-file-format
                                (file-name-base ifile))
                        "." (nth 1 to-spec)))
         (command (format-spec (nth 3 from-spec)
                               (list (cons ?i ifile)
                                     (cons ?o ofile)
                                     (cons ?O (file-name-base ofile))
                                     (cons ?t (nth 3 to-spec))))))
    (unless to-spec
      (error "'to' spec %s is not defined for this exporter '%s'"
             to (pm--object-name exporter)))
    ;; compunicate with sentinel and callback with local vars in order to
    ;; avoid needless clutter
    ;; fixme: use -hist
    (set (make-local-variable 'pm--output-file) ofile)
    (set (make-local-variable 'pm--input-file) ifile)
    (message "Exporting '%s' with '%s' exporter ..."
             (file-name-nondirectory ifile) (pm--object-name exporter))
    (let ((ofile  (funcall (oref exporter :function) command from to)))
      (and ofile (stringp ofile) (pm--display-file ofile)))))

(defmacro pm--export-wrap-callback (slot)
  ;; replace exporter's :sentinel or :callback temporally in order to display
  ;; the output buffer
  `(let ((sentinel1 (oref exporter ,slot)))
     (condition-case err
         (let ((sentinel2 `(lambda (proc name)
                             (let ((efile (,sentinel1 proc name)))
                               (pm--display-file efile)
                               efile))))
           (oset exporter ,slot sentinel2)
           (call-next-method exporter from to ifile))
       (error (oset exporter ,slot sentinel1)
              (signal (car err) (cdr err))))
     (oset exporter ,slot sentinel1)))

(defmethod pm-export ((exporter pm-callback-exporter) from to &optional ifile)
  (pm--export-wrap-callback :callback))

(defmethod pm-export ((exporter pm-shell-exporter) from to &optional ifile)
  (pm--export-wrap-callback :sentinel))


;; UI
(defvar pm--exporter-hist nil)
(defvar pm--export:from-hist nil)
(defvar pm--export:to-hist nil)

(defun polymode-export (&optional from to)
  "Export current file.

FROM and TO are the from-id and to-id in the definition of the
current exporter. If current exporter hasn't been set yet, call
`polymode-set-exporter' before exporting.

If called interactively with C-u argument, ask for the FROM type
interactively, otherwise FROM and TO are determined automatically
from the current exporter specification and current file
extension.  See `pm-exporter' for the precise specification.

If called interactively with C-u C-u argument, set new exporter
first with `polymode-set-exporter'."
  (interactive "P")
  ;; todo: '(16) should allow for edditing :from command
  (let* ((exporter (symbol-value
                    (if (equal from '(16))
                        (polymode-set-exporter)
                      (or (oref pm/config :exporter)
                          (polymode-set-exporter)))))
         (fname (file-name-nondirectory buffer-file-name))
         (e:from (oref exporter :from))
         (e:to (oref exporter :to))
         (from-opts (mapcar (lambda (el)
                              (propertize (nth 2 el) :id (car el)))
                            (oref exporter :from)))
         (to-opts (mapcar (lambda (el)
                            (propertize (format "%s (%s)" (nth 2 el) (nth 1 el))
                                        :id (car el)))
                          (oref exporter :to)))
         (from
          (cond ((or (null from) (equal from '(16)))
                 (let ((case-fold-search  t))
                   (or (and (pm--get-hist :export-from)
                            (get-text-property 0 :id (pm--get-hist :export-from)))
                       (car (cl-rassoc-if (lambda (el)
                                            (string-match-p (car el) fname))
                                          e:from))
                       ;; guess from weaver and return a cons of (weaver-id . exporter-id)
                       (let ((weaver (symbol-value (or (oref pm/config :weaver)
                                                       (polymode-set-weaver)))))
                         (cl-loop for w in (oref weaver :from-to)
                                  ;; weaver intput extension matches the filename
                                  if (string-match-p (nth 1 w) fname) 
                                  return (cl-loop for e in e:from
                                                  ;; input exporter extensnion matches weaver output extension
                                                  if (string-match-p (nth 1 e) (concat "dummy." (nth 2 w)))
                                                  return (cons (car w) (car e)))))
                       (let* ((prompt (format "No input spec for extension '.%s' in '%s' exporter. Choose one: "
                                              (file-name-extension fname)
                                              ;; object-name returns clutter like "#<pm-exporter pandoc>"
                                              ;; use internal implementation:
                                              (if (fboundp 'eieio--object-name)
                                                  (eieio--object-name exporter)
                                                (aref exporter object-name))))
                              (sel (ido-completing-read prompt from-opts nil t nil
                                                        'pm--export:from-hist
                                                        (pm--get-hist :export-from))))
                         (pm--put-hist :export-from sel)
                         (get-text-property 0 :id sel)))))
                ;; C-u, force a :from spec
                ((equal from '(4))
                 (let ((sel (ido-completing-read "Input type: " from-opts nil t nil
                                                 'pm--export:from-hist (pm--get-hist :export-from)) ))
                   (pm--put-hist :export-from sel)
                   (get-text-property 1 :id sel)))
                ((stringp from)
                 (if (assoc from e:from)
                     from
                   (error "Cannot find input spec '%s' in %s exporter" from (object-name exporter))))
                (t (error "'from' argument must be nil, universal argument or a string"))
                ))
         (to
          (cond ((null to)
                 (let ((sel (ido-completing-read "Export to: " to-opts nil t nil
                                                 'pm--export:to-hist (pm--get-hist :export-to))))
                   (pm--put-hist :export-to sel)
                   (get-text-property 1 :id sel)))
                ((stringp to)
                 (if (assoc to e:to)
                     to
                   (error "Cannot find output spec '%s' in %s exporter" to (object-name exporter))))
                (t (error "'to' argument must be nil or a string")))))
    (if (consp from)
        ;; run through weaver
        (pm-weave (symbol-value (oref pm/config :weaver)) (car from) (cons (cdr from) to))
      (pm-export exporter from to))))

(defmacro polymode-register-exporter (exporter default? &rest configs)
  "Add EXPORTER to :exporters slot of all config objects in CONFIGS.
When DEFAULT? is non-nil, also make EXPORTER the default exporter
for each polymode in CONFIGS."
  `(dolist (pm ',configs)
     (object-add-to-list (symbol-value pm) :exporters ',exporter)
     (when ,default? (oset (symbol-value pm) :exporter ',exporter))))

(defun polymode-set-exporter ()
  (interactive)
  (unless pm/config
    (error "No pm/config object found. Not in polymode buffer?"))
  (let* ((exporters (pm--abrev-names
                     (delete-dups (pm--oref-with-parents pm/config :exporters))
                     "pm-exporter/"))
         (sel (ido-completing-read "No default exporter. Choose one: " exporters nil t nil
                                   'pm--exporter-hist (car pm--exporter-hist)))
         (out (intern (get-text-property 0 :orig sel))))
    (oset pm/config :exporter out)
    out))


;;; UTILS
(defun pm-default-export-sentinel (process name)
  "Default exporter sentinel."
  (pm--run-command-sentinel process name "exporting"))

(defun pm-default-shell-export-function (command from to)
  "Run exporting command interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts
user interaction. This is a default function in all exporters
that call a shell command"
  (pm--run-command command
                   (oref (symbol-value (oref pm/config :exporter)) :sentinel)  
                   "*polymode export*"
                   (concat "Exporting " from "-->" to
                           " with command:\n     " command "\n")))


;;; GLOBAL EXPORTERS
(defcustom pm-exporter/pandoc
  (pm-shell-exporter "pandoc"
               :from
               '(;; ("json"	"\\.json\\'" "JSON native AST"	"pandoc %i -f json -t %t -o %o")
                 ("markdown"	"\\.md\\'" "pandoc's markdown" 	"pandoc %i -f markdown -t %t -o %o")
                 ("markdown_strict"	"\\.md\\'" "original markdown"	"pandoc %i -f markdown_strict -t %t -o %o")
                 ("markdown_phpextra"	"\\.md\\'" "PHP markdown"	"pandoc %i -f markdown_phpextra -t %t -o %o")
                 ("markdown_phpextra"	"\\.md\\'" "github markdown" 	"pandoc %i -f markdown_phpextra -t %t -o %o")
                 ("textile"	"\\.textile\\'" "Textile" 		"pandoc %i -f textile -t %t -o %o")
                 ("rst"		"\\.rst\\'" "reStructuredText"	"pandoc %i -f rst -t %t -o %o")
                 ("html"	"\\.x?html?\\'" "HTML" 	"pandoc %i -f html -t %t -o %o")
                 ("doocbook"	"\\.xml\\'" "DocBook" 		"pandoc %i -f doocbook -t %t -o %o")
                 ("mediawiki"	"\\.wiki\\'" "MediaWiki"		"pandoc %i -f mediawiki -t %t -o %o")
                 ("latex"	"\\.tex\\'" "LaTeX" 		"pandoc %i -f latex -t %t -o %o")
                 )
               :to
               '(;; ("json"  	"json"  "JSON version of native AST" "json")
                 ("plain"  	"txt"  "plain text" "plain")
                 ("markdown"  	"md"  "pandoc's extended markdown" "markdown")
                 ("markdown_strict"  	"md"  "original markdown" "markdown_strict")
                 ("markdown_phpextra"  	"md"  "PHP extended markdown" "markdown_phpextra")
                 ("markdown_github"  	"md"  "github extended markdown" "markdown_github")
                 ("rst"  	"rst"  "reStructuredText" "rst")
                 ("html"  	"html"  "XHTML 1" "html")
                 ("html5"  	"html"  "HTML 5" "html5")
                 ("latex"  	"tex"  "LaTeX" "latex")
                 ("beamer"  	"tex"  "LaTeX beamer" "beamer")
                 ("context"  	"tex"  "ConTeXt" "context")
                 ("man"  	"man"  "groff man" "man")
                 ("mediawiki"  	"wiki"  "MediaWiki markup" "mediawiki")
                 ("textile"  	"textile"  "Textile" "textile")
                 ("org"  	"org"  "Emacs Org-Mode" "org")
                 ("texinfo"  	"info"  "GNU Texinfo" "texinfo")
                 ("docbook"  	"xml"  "DocBook XML" "docbook")
                 ("opendocument"  	"xml"  "OpenDocument XML" "opendocument")
                 ("odt"  	"odt"  "OpenOffice text document" "odt")
                 ("docx"  	"docx"  "Word docx" "docx")
                 ("epub"  	"epub"  "EPUB book" "epub")
                 ("epub3"  	"epub"  "EPUB v3" "epub3")
                 ("fb2"  	"fb"  "FictionBook2 e-book" "fb2")
                 ("asciidoc"  	"txt"  "AsciiDoc" "asciidoc")
                 ("slidy"  	"html"  "Slidy HTML slide show" "slidy")
                 ("slideous"  	"html"  "Slideous HTML slide show" "slideous")
                 ("dzslides"  	"html"  "HTML5 slide show" "dzslides")
                 ("s5"  	"html"  "S5 HTML slide show" "s5")
                 ("rtf"  	"rtf"  "rich text format" "rtf")
                 )
               :function 'pm-default-shell-export-function
               :sentinel 'pm-default-export-sentinel)
  "Pandoc exporter"
  :group 'polymode-export
  :type 'object)


