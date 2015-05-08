(require 'poly-mode)
(require 'poly-block-matchers)

(defcustom pm-host/slim
  (pm-bchunkmode "slim" :mode 'slim-mode)
  "slim host chunkmode"
  :group 'hostmodes
  :type 'object)

(pm-create-block-matchers "slim-coffee" "^ *coffee: *$")
(defcustom pm-inner/slim-coffee
  (pm-hbtchunkmode "slim coffee include"
                   :mode 'coffee-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-coffee-head-matcher
                   :tail-reg  'pm-slim-coffee-tail-matcher)
  "erb typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-block-matchers "slim-ruby" "^ *ruby: *$")
(defcustom pm-inner/slim-ruby
  (pm-hbtchunkmode "slim ruby include"
                   :mode 'enh-ruby-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-ruby-head-matcher
                   :tail-reg  'pm-slim-ruby-tail-matcher)
  "slim embedded ruby typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-block-matchers "slim-js" "^ *javascript: *$")
(defcustom pm-inner/slim-js
  (pm-hbtchunkmode "slim js include"
                   :mode 'js-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-js-head-matcher
                   :tail-reg  'pm-slim-js-tail-matcher)
  "erb typical chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/slim
  (pm-polymode-multi "slim"
                     :hostmode 'pm-host/slim
                     :innermodes '(pm-inner/slim-coffee
                                   pm-inner/slim-js
                                   pm-inner/slim-ruby))

  "slim typical polymode."
  :group 'polymodes
  :type 'object)
(define-polymode poly-slim-mode pm-poly/slim)

(provide 'poly-slim)
