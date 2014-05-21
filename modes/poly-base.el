
;; BASE MODES
(defcustom pm-base/blank
  (pm-basemode "blank")
  "Blank submode. This is the default :basemode for all pm-polymode objects.
On initalisation this submode sets :mode to whatever major-mode
is in place at that time."
  :group 'basemodes
  :type 'object)

(defcustom pm-base/fundamental
  (pm-basemode "fundamental"
              :mode 'fundamental-mode)
  "Fundamental base mode"
  :group 'basemodes
  :type 'object)

(defcustom pm-base/latex
  (pm-basemode "latex"
              :mode 'latex-mode)
  "Latex base submode"
  :group 'basemodes
  :type 'object)

(defcustom pm-base/html
  (pm-basemode "html"
              :mode 'html-mode)
  "HTML base submode"
  :group 'basemodes
  :type 'object)

(defcustom pm-base/R
  (pm-basemode "R"
              :mode 'R-mode)
  "R base submode"
  :group 'basemodes
  :type 'object)

(defcustom pm-base/C++
  (pm-basemode "C++"
              :mode 'c++-mode
              :font-lock-narrow nil)
  "C++ base submode"
  :group 'basemodes
  :type 'object)

(defcustom pm-base/text
  (pm-basemode "text"
              :mode 'text-mode)
  "Text base submode"
  :group 'basemodes
  :type 'object)

(defcustom pm-base/yaml
  (pm-basemode "YAML"
              :mode 'yaml-mode)
  "YAML submode"
  :group 'basemodes
  :type 'object)



;;; CONFIG objects
;; These are simple generic configuration objects. More specialized configuration
;; objects are defined in mode-specific files (e.g. poly-R.el, poly-markdown.el etc).
(defcustom pm-poly/brew
  (pm-polymode-one "brew"
                 :basemode 'pm-base/text
                 :chunkmode 'pm-chunk/fundamental)
  "Typical Brew configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/html
  (pm-polymode-one "html"
                 :basemode 'pm-base/html
                 :chunkmode 'pm-chunk/fundamental)
  "HTML typical configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/C++
  (pm-polymode-one "C++"
                 :basemode 'pm-base/C++
                 :chunkmode 'pm-chunk/fundamental)
  "C++ typical configuration"
  :group 'polymodes
  :type 'object)


(provide 'poly-base)
