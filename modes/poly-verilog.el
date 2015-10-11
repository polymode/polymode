;;; poly-PL.el
;;
;; Author: Rado Rodopski

(require 'polymode)

(defcustom pm-host/verilog
  (pm-bchunkmode "Verilog" :mode 'verilog-mode)
  "Verilog host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom pm-inner/perl-block
  (pm-hbtchunkmode "perl-block"
                   :mode 'perl-mode
                   :head-mode 'body
                   :tail-mode 'body
                   :head-reg  "<perl>"
                   :tail-reg  "</perl>")
  "Perl typical block chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-inner/perl-eval
  (pm-hbtchunkmode "perl-eval"
                   :mode 'perl-mode
                   :head-mode 'body
                   :tail-mode 'body
                   :head-reg  "<%"
                   :tail-reg  "%>")
  "Perl typical eval chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-inner/perl-line
  (pm-hbtchunkmode "perl-line"
                   :mode 'perl-mode
                   :head-mode 'host
                   :tail-mode 'host
                   :head-reg  "<pl>"
                   :tail-reg  "$")
  "Perl typical line chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-inner/perl-inline-var
  (pm-hbtchunkmode "perl-inline-var"
                   :mode 'perl-mode
                   :head-mode 'body
                   :tail-mode 'body
                   :head-reg  "<\\$+"
                   :tail-reg  ">")
  "Perl typical inline variable chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/verilog+perl
  (pm-polymode-multi "verilog+perl"
					 :hostmode 'pm-host/verilog
					 :innermodes '(pm-inner/perl-block
								   pm-inner/perl-eval
								   pm-inner/perl-line
								   pm-inner/perl-inline-var))
  "verilog-perl typical polymode."
  :group 'polymodes
  :type 'object)


(define-polymode poly-verilog+perl-mode pm-poly/verilog+perl)


(provide 'poly-verilog)
