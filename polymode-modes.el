;; BASE
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

;; NOWEB
(defcustom  pm-submode/noweb
  (pm-inner-submode "noweb"
                    :head-reg  "^<<\\(.*\\)>>="
                    :tail-reg    "^\\(@ +%def .*\\)$\\|\\(@[ \n]\\)"
                    :font-lock-keywords `(("^\\(<<\\)\\(.*\\)\\(>>=\\)$" (1 'font-lock-keyword-face)
                                           (2 'font-lock-variable-name-face) (3 'font-lock-keyword-face))
                                          ("^\\(@ +%def\\) +\\(.+\\)"
                                           (1 'font-lock-keyword-face)
                                           (2 'font-lock-variable-name-face))
                                          "^@")
                    :font-lock-matcher '("\\(?:$\\|[^@]\\|\\`\\)\\(<<\\)\\([^>]+\\)\\(>>\\)"
                                         (1 'font-lock-keyword-face t)
                                         (2 'font-lock-variable-name-face t)
                                         (3 'font-lock-keyword-face t))
                    :font-lock-syntactic-matcher '("\\(?:$\\|[^@]\\)\\(<\\)<[^>]+>\\(>\\)" (1 "!") (2 "!"))
                    :font-lock-literal-matcher '("\\(\\[\\)\\[[^]]+]\\(]\\)" (1 "|") (2 "|")))
  "Noweb typical chunk."
  :group 'polymode
  :type 'object)

(defcustom pm-config/noweb
  (pm-config-one "noweb"
                 :base-submode-name 'pm-base/latex
                 :inner-submode-name 'pm-submode/noweb)
  "Noweb typical configuration"
  :group 'polymode
  :type 'object)

;; (setq config pm-config/noweb)
(defcustom pm-submode/noweb-R
  (clone pm-submode/noweb :mode 'R-mode)
  "Noweb for R"
  :group 'polymode
  :type 'object)

(defcustom pm-config/noweb-R
  (clone pm-config/noweb :inner-submode-name 'pm-submode/noweb-R)
  "Noweb for R configuration"
  :group 'polymode
  :type 'object)


;; (defcustom lp-noweb-chunk:knitr
;;   (litprog-chunk "knitr-chunk"
;;                  :mode nil
;;                  :start  "^[ \t]*<<\\(.*\\)>>="
;;                  :end    "^[ \t]*\\(@ +%def .*\\)$\\|\\(@[ \n]\\)"
;;                  )
;;   "Knitr allows for spaces before chunks. Useful for indentation."
;;   :group 'lp-noweb
;;   :type 'object)

(provide 'polymode-modes)
