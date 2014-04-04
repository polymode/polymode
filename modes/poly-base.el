(require 'polymode)


;; BASE MODES
(defcustom pm-base/blank
  (pm-submode "blank")
  "Blank submode. This is the default :basemode for all pm-config objects.
On initalisation this submode sets :mode to whatever major-mode
is in place at that time."
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/fundamental
  (pm-submode "fundamental"
              :mode 'fundamental-mode)
  "Fundamental base mode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/latex
  (pm-submode "latex"
              :mode 'latex-mode)
  "Latex base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/markdown
  (pm-submode "Markdown"
              :mode 'markdown-mode)
  "Markdown base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/html
  (pm-submode "html"
              :mode 'html-mode)
  "HTML base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/R
  (pm-submode "R"
              :mode 'R-mode)
  "R base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/C++
  (pm-submode "C++"
              :mode 'c++-mode
              :font-lock-narrow nil)
  "C++ base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/text
  (pm-submode "text"
              :mode 'text-mode)
  "Text base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/yaml
  (pm-submode "YAML"
              :mode 'yaml-mode)
  "YAML submode"
  :group 'polymode-basemodes
  :type 'object)



;;; CONFIG objects
;; These are simple generic configuration objects. More specialized configuration
;; objects are defined in mode-specific files (e.g. poly-R.el, poly-markdown.el etc).
(defcustom pm-config/brew
  (pm-config-one "brew"
                 :basemode-name 'pm-base/text
                 :innermode-name 'pm-submode/fundamental)
  "Typical Brew configuration"
  :group 'polymode-config
  :type 'object)

(defcustom pm-config/html
  (pm-config-one "html"
                 :basemode-name 'pm-base/html
                 :innermode-name 'pm-submode/fundamental)
  "HTML typical configuration"
  :group 'polymode-config
  :type 'object)

(defcustom pm-config/C++
  (pm-config-one "C++"
                 :basemode-name 'pm-base/C++
                 :innermode-name 'pm-submode/fundamental)
  "C++ typical configuration"
  :group 'polymode-config
  :type 'object)


(provide 'poly-base)
