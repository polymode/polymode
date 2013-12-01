(require 'polymode)

(defcustom pm-config/noweb
  (pm-config-one "noweb"
                 :base-submode-name 'pm-base/latex
                 :inner-submode-name 'pm-submode/noweb
                 :map '(("<" . poly-noweb-electric-<)))
  "Noweb typical configuration"
  :group 'polymode
  :type 'object)

(defcustom  pm-submode/noweb
  (pm-inner-submode "noweb"
                    :head-reg  "<<\\(.*\\)>>="
                    :tail-reg    "\\(@ +%def .*\\)$\\|\\(@[ \n]\\)")
  "Noweb typical chunk."
  :group 'polymode
  :type 'object)


(define-polymode poly-noweb-mode pm-config/noweb)

(defun poly-noweb-electric-< (arg)
  "Auto insert noweb chunk if at bol followed by white space.
If given an numerical argument, it simply insert `<'. Otherwise,
if at the beginning of a line in a base chunk insert \"<<>>=\", a
closing \"@\" and a newline if necessary."
  (interactive "P")
  (if (or arg (not (eq pm/type 'base)))
      (self-insert-command (if (numberp arg) arg 1))
    (if (not (looking-back "^[ \t]*"))
        (self-insert-command 1)
      (insert "<<")
      (save-excursion
        (insert ">>=\n\n@ ")
            (unless(looking-at "\\s *$")
                (newline)))
          (ess-noweb-update-chunk-vector))))

(provide 'poly-noweb)
