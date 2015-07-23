(defmacro pm-create-indented-block-matchers (name regex)
  "Defines 2 functions, each return a list of the start and end points of the HEAD
and TAIL portions of an indented block of interest, via some regex. You can thene
use these functions in the (defcustom pm-inner/<lang

eg (pm-create-indented-block-matchers 'slim-coffee' \"^[^ ]*\\(.*:? *coffee: *\\)$\")

creates the functions

pm-slim-coffee-head-matcher
pm-slim-coffee-tail-matcher

In the example below,

The head matcher will match against 'coffee:', returning the positions of the start
and end of 'coffee:'
The tail matcher will return a list (n, n) of the final characters is the block.

    |<----- Uses this indentation to define the left edge of the 'block'
    |
    |<--->|  This region is higlighted by the :head-mode in the block-matchers
    |     |
    |     | the head matcher uses this column as the start position of the head
    |     |<----- the head matcher uses this column as the end position of the head
    |     |
``` :     :
1|  :     :
2|  coffee:
3|    myCoffeeCode()
4|    moreCode ->
5|      do things
6|              :
7|  This is no longer in the block
8|              :
```             |
               >|<----- this region of 0 width is highlighted by the :tail-mode
                        the 'block' ends after this column on line 5


All the stuff after the -end- of the head and before the start of the tail is
sent to the new mode for syntax highlighting
"
  (let* ((head-name (intern (format "pm-%s-head-matcher" name)))
         (tail-name (intern (format "pm-%s-tail-matcher" name))))
    `(progn
       (defun ,head-name (ahead)
         (when (re-search-forward ,regex nil t ahead)
           (cons (match-beginning 1) (match-end 1))))

       (defun ,tail-name (ahead)
         (save-excursion
           ;; (cons (point-max) (point-max)))))))
           (goto-char (car (,head-name 1)))
           (let* ((block-col (current-indentation))
                  (posn (catch 'break
                          (while (not (eobp))
                            (forward-line 1)
                            (when (and (<= (current-indentation) block-col)
                                       (not (progn
                                              (beginning-of-line)
                                              (looking-at "^[[:space:]]*$"))))
                              (throw 'break (point-at-bol))))
                          (throw 'break (point-max)))))
             (cons posn posn)))))))

(provide 'poly-block-matchers)
