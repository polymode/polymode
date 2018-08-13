
(require 'polymode)
;; (require 'ruby-mode)

(defcustom pm-inner/erb
  (pm-inner-chunkmode "erb"
                      :mode 'ruby-mode
                      :head-matcher  "\"?\<\% *[-=]?"
                      :tail-matcher  "\%\>\"?")
  "Erb typical chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/coffee+erb
  (pm-polymode "coffee+erb"
               :hostmode 'pm-host/coffee
               :innermodes '(pm-inner/erb))
  "coffee-erb typical polymode."
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-coffee+erb-mode "poly-erb")
(define-polymode poly-coffee+erb-mode pm-poly/coffee+erb)

(defcustom pm-poly/js+erb
  (pm-polymode-one "js+erb"
                   :hostmode 'pm-host/js
                   :innermodes '(pm-inner/erb))
  "Javascript-erb typical polymode."
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-javascript+erb-mode "poly-erb")
(define-polymode poly-js+erb-mode pm-poly/js+erb)

(defcustom pm-poly/html+erb
  (pm-polymode-one "html+erb"
                   :hostmode 'pm-host/html
                   :innermode 'pm-inner/erb)
  "html-erb typical polymode."
  :group 'polymodes
  :type 'object)

;;;###autoload  (autoload 'poly-html+erb-mode "poly-erb")
(define-polymode poly-html+erb-mode pm-poly/html-erb)

(provide 'poly-erb)
