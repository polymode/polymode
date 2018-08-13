 ;;; CORE HOST AND POLYMODE OBJECTS


;; HOST MODES

(defcustom pm-host/blank
  (pm-host-chunkmode "FallBack" :mode nil)
  "Blank. Used as a placeholder for currently installed mode."
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/coffee
  (pm-host-chunkmode "coffee" :mode 'coffee-mode)
  "Coffee host chunkmode."
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/fallback
  (pm-host-chunkmode "FallBack" :mode 'poly-fallback-mode)
  "Polymode fall back host mode."
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/fundamental
  (pm-host-chunkmode "fundamental" :mode 'fundamental-mode)
  "Fundamental host mode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/js
  (pm-host-chunkmode "js" :mode 'js-mode)
  "Javascript host chunkmode."
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/latex
  (pm-host-chunkmode "latex" :mode 'latex-mode)
  "Latex host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/html
  (pm-host-chunkmode "html" :mode 'html-mode)
  "HTML host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/R
  (pm-host-chunkmode "R" :mode 'R-mode)
  "R host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/C++
  (pm-host-chunkmode "C++"
                     :mode 'c++-mode
                     :font-lock-narrow nil)
  "C++ host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/text
  (pm-host-chunkmode "text" :mode 'text-mode)
  "Text host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/yaml
  (pm-host-chunkmode "YAML" :mode 'yaml-mode)
  "YAML chunkmode"
  :group 'hostmodes
  :type 'object)



;;; ROOT POLYMODES

;; These are simple generic configuration objects. More specialized polymodes
;; should clone these.

(defcustom pm-poly/brew
  (pm-polymode "brew"
               :hostmode 'pm-host/text
               :innermodes nil)
  "Typical Brew configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/html
  (pm-polymode "html"
               :hostmode 'pm-host/html
               :innermodes nil)
  "HTML typical configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/C++
  (pm-polymode "C++"
               :hostmode 'pm-host/C++
               :innermodes nil)
  "C++ typical configuration"
  :group 'polymodes
  :type 'object)

(provide 'poly-base)
