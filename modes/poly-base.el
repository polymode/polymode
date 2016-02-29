;;; CORE POLYMODE AND HOST OBJECTS


;;; POLYMODE objects
;; These are simple generic configuration objects. More specialized
;; configuration objects are defined in language-specific files (e.g. poly-R.el,
;; poly-markdown.el etc).

(defcustom pm-inner/fallback
  (pm-chunkmode "FallBack" :mode 'poly-fallback-mode)
  "Polymode fall back inner mode."
  :group 'hostmodes
  :type 'object)

(defcustom pm-poly/brew
  (pm-polymode-one "brew"
                 :hostmode 'pm-host/text
                 :innermode 'pm-inner/fallback)
  "Typical Brew configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/html
  ;; fixme: should probably be pm-polymode-multi
  (pm-polymode-one "html"
                 :hostmode 'pm-host/html
                 :innermode 'pm-inner/fallback)
  "HTML typical configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-poly/C++
  (pm-polymode-one "C++"
                 :hostmode 'pm-host/C++
                 :innermode 'pm-inner/fallback)
  "C++ typical configuration"
  :group 'polymodes
  :type 'object)



;; HOST MODES

(defcustom pm-host/blank
  (pm-bchunkmode "FallBack" :mode nil)
  "Blank. Used as a placeholder for currently installed mode.
It is specifically intended to be used with minor modes."
  :group 'hostmodes
  :type 'object)

(defcustom pm-host/fallback
  (pm-bchunkmode "FallBack"
                 :mode 'poly-fallback-mode)
  "Polymode fall back host mode."
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
