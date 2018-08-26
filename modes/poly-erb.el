
(require 'polymode)
;; (require 'ruby-mode)

(defcustom pm-inner/erb
  (pm-inner-chunkmode "erb"
                      :mode 'ruby-mode
                      :head-matcher  "\"?\<\% *[-=]?"
                      :tail-matcher  "\%\>\"?")
  "Erb typical chunk."
  :group 'poly-inner-modes
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

(add-to-list 'auto-mode-alist '("\\.js.erb$" . poly-javascript+erb-mode))
(add-to-list 'auto-mode-alist '("\\.coffee.erb$" . poly-coffee+erb-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . poly-html+erb-mode))

(provide 'poly-erb)
