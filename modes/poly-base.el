;;; CORE POLYMODE AND HOST OBJECTS


;;; POLYMODE objects
;; These are simple generic configuration objects. More specialized
;; configuration objects are defined in language-specific files (e.g. poly-R.el,
;; poly-markdown.el etc).
(defcustom pm-poly/brew
  (pm-polymode-one "brew"
                 :hostmode 'pm-host/text
                 :innermode 'pm-inner/fundamental)
  "Typical Brew configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/html
  ;; fixme: should probably be pm-polymode-multi
  (pm-polymode-one "html"
                 :hostmode 'pm-host/html
                 :innermode 'pm-inner/fundamental)
  "HTML typical configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/C++
  (pm-polymode-one "C++"
                 :hostmode 'pm-host/C++
                 :innermode 'pm-inner/fundamental)
  "C++ typical configuration"
  :group 'polymodes
  :type 'object)



;; HOST MODES
(defcustom pm-host/blank
  (pm-bchunkmode "blank")
  "Blank chunkmode. This is the default :hostmode for all pm-polymode objects.
On initalisation this chunkmode sets :mode to whatever major-mode
is in place at that time."
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/fundamental
  (pm-bchunkmode "fundamental"
                 :mode 'fundamental-mode)
  "Fundamental host mode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/latex
  (pm-bchunkmode "latex"
                 :mode 'latex-mode)
  "Latex host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/html
  (pm-bchunkmode "html"
                 :mode 'html-mode)
  "HTML host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/R
  (pm-bchunkmode "R"
                 :mode 'R-mode)
  "R host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/C++
  (pm-bchunkmode "C++"
                 :mode 'c++-mode
                 :font-lock-narrow nil)
  "C++ host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/text
  (pm-bchunkmode "text"
                 :mode 'text-mode)
  "Text host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/yaml
  (pm-bchunkmode "YAML"
                 :mode 'yaml-mode)
  "YAML chunkmode"
  :group 'hostmodes
  :type 'object)


(provide 'poly-base)
