(defgroup polymode-export nil
  "Polymode Exporters"
  :group 'polymode)

(defcustom polymode-exporter-output-file-format "%s[exported]"
  "Format of the exported files.
%s is substituted with the current file name sans extension.")

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
    :initform nil
    :type (or null function)
    :documentation
    "Function to process the commmand. Must take 3 arguments
     COMMAND, FROM-ID and TO-ID. COMMAND is the 4th argument
     of :from spec with all the formats substituted. FROM-ID is
     the id of requested :from spec, TO-ID is the id of the :to
     spec.")
   (sentinel
    :initarg :sentinel
    :initform nil
    :type (or null function)
    :documentation
    "Optional sentinel function to be called by :function when a
    shell call is involved."))
  "Root exporter class.")


;; UI
(defvar pm--exporter-hist nil)
(defvar pm--export:from-hist nil)
(defvar pm--export:to-hist nil)

(defun polymode-export (&optional from to)
  "todo:"
  ;; todo: '(16) should allow for edditing :from command
  (interactive "P")
  (let* ((exporter (symbol-value (or (oref pm/config :exporter)
                                     (polymode-set-exporter))))
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
          (cond ((null from)
                 (let ((fname (file-name-nondirectory buffer-file-name)))
                   (or (and (pm--get-hist :export-from)
                            (get-text-property 0 :id (pm--get-hist :export-from)))
                       (car (cl-rassoc-if (lambda (el)
                                            (string-match-p (car el) fname))
                                          e:from))
                       (let* ((prompt (format "No input spec for '%s'. Choose one: "
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
                 (let ((sel (ido-completing-read "Choose output spec: " to-opts nil t nil
                                                 'pm--export:to-hist (pm--get-hist :export-to))))
                   (pm--put-hist :export-to sel)
                   (get-text-property 1 :id sel)))
                ((stringp to)
                 (if (assoc to e:to)
                     to
                   (error "Cannot find output spec '%s' in %s exporter" to (object-name exporter))))
                (t (error "'to' argument must be nil or a string"))))
         (from-spec (assoc from e:from))
         (to-spec (assoc to e:to))
         (to-file (concat (format polymode-exporter-output-file-format
                                  (file-name-base buffer-file-name))
                          "." (nth 1 to-spec)))
         (command (format-spec (nth 3 from-spec)
                               (list (cons ?i (file-name-nondirectory buffer-file-name))
                                     (cons ?o to-file)
                                     (cons ?t (nth 3 to-spec))))))
    ;; compunicate with sentinel with local vars to avoid needless clutter
    (set (make-local-variable 'pm--output-file) to-file)
    (funcall (oref exporter :function) command from to)))

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
                                   'pm--exporter-hist (oref pm/config :exporter)))
         (out (intern (get-text-property 0 :orig sel))))
    (oset pm/config :exporter out)
    out))


;;; UTILS
(defun pm-default-export-sentinel (process name)
  "Default exporter sentinel."
  (pm--run-command-sentinel process name t "exporting"))

(defun pm-default-export-function (command from to)
  "Run exporting command interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts
user interaction. This is a default function in all exporters
that call a shell command"
  (pm--run-command command
                   (oref (symbol-value (oref pm/config :exporter))
                         :sentinel)  
                   "*polymode export*"
                   (concat "Exporting " from "-->" to
                           " with command:\n     " command "\n")))


;;; GLOBAL EXPORTERS
(defcustom pm-exporter/pandoc
  (pm-exporter "pandoc"
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
               :function 'pm-default-export-function
               :sentinel 'pm-default-export-sentinel)
  "Pandoc exporter"
  :group 'polymode-export
  :type 'object)
