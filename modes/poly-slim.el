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
  (pm-host-chunkmode "slim" :mode 'slim-mode)
  "slim host chunkmode"
  :group 'hostmodes
  :type 'object)

(pm-create-indented-block-matchers "slim-coffee" "^[^ ]*\\(.*:? *coffee: *\\)$")
(defcustom pm-inner/slim-coffee
  (pm-inner-chunkmode "slim coffee include"
                      :mode 'coffee-mode
                      :head-mode 'slim-mode
                      :tail-mode 'slim-mode
                      :head-matcher  'pm-slim-coffee-head-matcher
                      :tail-matcher  'pm-slim-coffee-tail-matcher)
  "slim-coffee typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-css" "^[^ ]*\\(.*:? *css: *\\)$")
(defcustom pm-inner/slim-css
  (pm-inner-chunkmode "slim css include"
                      :mode 'css-mode
                      :head-mode 'slim-mode
                      :tail-mode 'slim-mode
                      :head-matcher  'pm-slim-css-head-matcher
                      :tail-matcher  'pm-slim-css-tail-matcher)
  "slim-css typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-scss" "^[^ ]*\\(.*:? *scss: *\\)$")
(defcustom pm-inner/slim-scss
  (pm-inner-chunkmode "slim scss include"
                      :mode 'scss-mode
                      :head-mode 'slim-mode
                      :tail-mode 'slim-mode
                      :head-matcher  'pm-slim-scss-head-matcher
                      :tail-matcher  'pm-slim-scss-tail-matcher)
  "slim-scss typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-ruby" "^[^ ]*\\(.*:? *ruby: *\\)$")
(defcustom pm-inner/slim-ruby
  (pm-inner-chunkmode "slim ruby include"
                      :mode 'ruby-mode
                      :head-mode 'slim-mode
                      :tail-mode 'slim-mode
                      :head-matcher  'pm-slim-ruby-head-matcher
                      :tail-matcher  'pm-slim-ruby-tail-matcher)
  "slim-ruby typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-js" "^[^ ]*\\(.*:? *javascript: *\\)$")
(defcustom pm-inner/slim-js
  (pm-inner-chunkmode "slim js include"
                      :mode 'js-mode
                      :head-mode 'slim-mode
                      :tail-mode 'slim-mode
                      :head-matcher  'pm-slim-js-head-matcher
                      :tail-matcher  'pm-slim-js-tail-matcher)
  "slim-js typical chunk."
  :group 'innermodes
  :type 'object)

(pm-create-indented-block-matchers "slim-md" "^[^ ]*\\(.*:? *markdown: *\\)$")
(defcustom pm-inner/slim-md
  (pm-inner-chunkmode "slim markdown include"
                      :mode 'markdown-mode
                      :head-mode 'slim-mode
                      :tail-mode 'slim-mode
                      :head-matcher  'pm-slim-md-head-matcher
                      :tail-matcher  'pm-slim-md-tail-matcher)
  "slim-markdown typical chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/slim
  (pm-polymode "slim"
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

;;;###autoload  (autoload 'poly-slim-mode "poly-slim")
(define-polymode poly-slim-mode pm-poly/slim)

(provide 'poly-slim)
