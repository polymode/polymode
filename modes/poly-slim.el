(require 'polymode)

;; We cannot have all these "requires" as part of polymode
;; https://github.com/vspinu/polymode/issues/69
;; (require 'css-mode)
;; (require 'scss-mode)
;; (require 'coffee-mode)
;; (require 'slim-mode)
;; (require 'ruby-mode)
;; (require 'markdown-mode)

(defconst poly-slim-engines (regexp-opt '("ruby" "javascript" "css" "sass" "scss"
                                          "less" "coffe" "markdown" "textile" "rdoc")))

(defcustom pm-host/slim
  (pm-host-chunkmode :object-name "slim"
                     :mode 'slim-mode)
  "slim host chunkmode"
  :group 'poly-host-modes
  :type 'object)

;; https://github.com/slim-template/slim/blob/master/README.md#embedded-engines-markdown-
(defcustom pm-inner/slim-code-block
  (pm-inner-auto-chunkmode :object-name "slim-code-block"
                           ;; not in comment
                           :head-matcher (cons (format "^[^/]*?\\(%s.*?:\\)" poly-slim-engines) 1)
                           :tail-matcher #'pm-same-indent-tail-matcher
                           :head-mode 'slim-mode
                           :indent-offset 4
                           :mode-matcher "[^ \t:]+")
  "Slim code block.
Slim code blocks are defined by the same level of
indentation (like python)."
  :group 'poly-inner-modes
  :type 'object)

;;;###autoload  (autoload 'poly-slim-mode "poly-slim")
(define-polymode poly-slim-mode
  :hostmode 'pm-host/slim
  :innermodes '(pm-inner/slim-code-block))

(add-to-list 'auto-mode-alist '("\\.slim$" . poly-slim-mode))

(provide 'poly-slim)
