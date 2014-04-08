(require 'polymode)

(defcustom pm-config/markdown
  (pm-config-multi-auto "markdown"
                        :basemode-name 'pm-base/markdown
                        :auto-chunkmode-name 'pm-chunk/markdown
                        :init-functions '(poly-markdown-remove-markdown-hooks))
  "Markdown typical configuration"
  :group 'polymode-configs
  :type 'object)

(defcustom  pm-chunk/markdown
  (pm-chunkmode-auto "markdown"
                         :head-reg "^[ \t]*```[{ \t]*\\w.*$"
                         :tail-reg "^[ \t]*```[ \t]*$"
                         :retriever-regexp "```[ \t]*{?\\(\\w+\\)"
                         :font-lock-narrow t)
  "Noweb typical chunk."
  :group 'polymode-chunkmodes
  :type 'object)

(define-polymode poly-markdown-mode pm-config/markdown)
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))


;;; FIXES:
(defun poly-markdown-remove-markdown-hooks ()
  ;; get rid of awfull hooks
  (remove-hook 'window-configuration-change-hook 'markdown-fontify-buffer-wiki-links t)
  (remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link t))


(provide 'poly-markdown)

