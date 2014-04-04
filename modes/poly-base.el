(require 'polymode)


;; BASE MODES
(defcustom pm-base/blank
  (pm-basemode "blank")
  "Blank submode. This is the default :basemode for all pm-config objects.
On initalisation this submode sets :mode to whatever major-mode
is in place at that time."
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/fundamental
  (pm-basemode "fundamental"
              :mode 'fundamental-mode)
  "Fundamental base mode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/latex
  (pm-basemode "latex"
              :mode 'latex-mode)
  "Latex base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/markdown
  (pm-basemode "Markdown"
              :mode 'markdown-mode)
  "Markdown base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/html
  (pm-basemode "html"
              :mode 'html-mode)
  "HTML base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/R
  (pm-basemode "R"
              :mode 'R-mode)
  "R base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/C++
  (pm-basemode "C++"
              :mode 'c++-mode
              :font-lock-narrow nil)
  "C++ base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/text
  (pm-basemode "text"
              :mode 'text-mode)
  "Text base submode"
  :group 'polymode-basemodes
  :type 'object)

(defcustom pm-base/yaml
  (pm-basemode "YAML"
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
                 :innermode-name 'pm-inner/fundamental)
  "Typical Brew configuration"
  :group 'polymode-config
  :type 'object)

(defcustom pm-config/html
  (pm-config-one "html"
                 :basemode-name 'pm-base/html
                 :innermode-name 'pm-inner/fundamental)
  "HTML typical configuration"
  :group 'polymode-config
  :type 'object)

(defcustom pm-config/C++
  (pm-config-one "C++"
                 :basemode-name 'pm-base/C++
                 :innermode-name 'pm-inner/fundamental)
  "C++ typical configuration"
  :group 'polymode-config
  :type 'object)


(provide 'poly-base)
