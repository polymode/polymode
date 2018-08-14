
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

;;;###autoload  (autoload 'poly-coffee+erb-mode "poly-erb")
(define-polymode poly-coffee+erb-mode
  :hostmode 'pm-host/coffee
  :innermodes '(pm-inner/erb))

;;;###autoload
(define-polymode poly-js+erb-mode
  :hostmode 'pm-host/js
  :innermodes '(pm-inner/erb))

;;;###autoload  (autoload 'poly-html+erb-mode "poly-erb")
(define-polymode poly-html+erb-mode
  :hostmode 'pm-host/html
  :innermode 'pm-inner/erb)

(provide 'poly-erb)
