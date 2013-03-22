(require 'polymode)

;; BASE MODES
(defcustom pm-base/fundamental
  (pm-submode "fundamental"
              :mode 'fundamental-mode)
  "Fundamental base mode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/latex
  (pm-submode "latex"
              :mode 'latex-mode)
  "Latex base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/markdown
  (pm-submode "latex"
              :mode 'markdown-mode)
  "Markdown base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/html
  (pm-submode "html"
              :mode 'html-mode)
  "HTML base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/R
  (pm-submode "R"
              :mode 'R-mode)
  "R base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/C++
  (pm-submode "C++"
              :mode 'c++-mode)
  "C++ base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/text
  (pm-submode "text"
              :mode 'text-mode)
  "Text base submode"
  :group 'base-submodes
  :type 'object)

(defcustom pm-base/ess-help
  (pm-submode "ess-help"
              :mode 'ess-help-mode)
  "ess-help"
  :group 'base-submodes
  :type 'object)

;; CONFIG
(defcustom pm-config/brew
  (pm-config-one "brew"
                 :base-submode-name 'pm-base/text
                 :inner-submode-name 'pm-submode/fundamental)
  "HTML typical configuration"
  :group 'polymode :type 'object)


(defcustom pm-config/html
  (pm-config-one "html"
                 :base-submode-name 'pm-base/html
                 :inner-submode-name 'pm-submode/fundamental)
  "HTML typical configuration"
  :group 'polymode :type 'object)



(provide 'poly-base)


