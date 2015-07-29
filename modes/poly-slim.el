(require 'polymode)

;; We cannot have all these "requires" as part of polymode
;; https://github.com/vspinu/polymode/issues/69
;; (require 'css-mode)
;; (require 'scss-mode)
;; (require 'coffee-mode)
;; (require 'slim-mode)
;; (require 'ruby-mode)
;; (require 'markdown-mode)

(defcustom pm-host/slim
  (pm-bchunkmode "slim" :mode 'slim-mode)
  "slim host chunkmode"
  :group 'hostmodes
  :type 'object)

(pm-create-indented-block-matchers "slim-coffee" "^[^ ]*\\(.*:? *coffee: *\\)$")
(defcustom pm-inner/slim-coffee
  (pm-hbtchunkmode "slim coffee include"
                   :mode 'coffee-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-coffee-head-matcher
                   :tail-reg  'pm-slim-coffee-tail-matcher)
  "slim-coffee typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-css" "^[^ ]*\\(.*:? *css: *\\)$")
(defcustom pm-inner/slim-css
  (pm-hbtchunkmode "slim css include"
                   :mode 'css-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-css-head-matcher
                   :tail-reg  'pm-slim-css-tail-matcher)
  "slim-css typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-scss" "^[^ ]*\\(.*:? *scss: *\\)$")
(defcustom pm-inner/slim-scss
  (pm-hbtchunkmode "slim scss include"
                   :mode 'scss-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-scss-head-matcher
                   :tail-reg  'pm-slim-scss-tail-matcher)
  "slim-scss typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-ruby" "^[^ ]*\\(.*:? *ruby: *\\)$")
(defcustom pm-inner/slim-ruby
  (pm-hbtchunkmode "slim ruby include"
                   :mode 'ruby-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-ruby-head-matcher
                   :tail-reg  'pm-slim-ruby-tail-matcher)
  "slim-ruby typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-js" "^[^ ]*\\(.*:? *javascript: *\\)$")
(defcustom pm-inner/slim-js
  (pm-hbtchunkmode "slim js include"
                   :mode 'js-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-js-head-matcher
                   :tail-reg  'pm-slim-js-tail-matcher)
  "slim-js typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-md" "^[^ ]*\\(.*:? *markdown: *\\)$")
(defcustom pm-inner/slim-md
  (pm-hbtchunkmode "slim markdown include"
                   :mode 'markdown-mode
                   :head-mode 'slim-mode
                   :tail-mode 'slim-mode
                   :head-reg  'pm-slim-md-head-matcher
                   :tail-reg  'pm-slim-md-tail-matcher)
  "slim-markdown typical chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/slim
  (pm-polymode-multi "slim"
                     :hostmode 'pm-host/slim
                     :innermodes '(pm-inner/slim-coffee
                                   pm-inner/slim-css
                                   pm-inner/slim-scss
                                   pm-inner/slim-js
                                   pm-inner/slim-md
                                   pm-inner/slim-ruby))

  "slim typical polymode."
  :group 'polymodes
  :type 'object)

(define-polymode poly-slim-mode pm-poly/slim)

(provide 'poly-slim)
