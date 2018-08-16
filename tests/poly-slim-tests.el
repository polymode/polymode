
(require 'coffee-mode)
(require 'css-mode)
(require 'markdown-mode)
(require 'ruby-mode)
(require 'slim-mode)

(require 'polymode-test)
(require 'poly-slim)

;; fixme: add indent tests

(ert-deftest poly-slim/changes ()
  (pm-test-poly-lock poly-slim-mode "slim.slim"
    ((delete-color ("color:" beg))
     (delete-backward-char 1))
    ((delete-markdown "markdown:")
     (backward-kill-word 1))
    ((insert-new-line ("## Subheading" end))
     (insert "\n"))))


(ert-deftest poly-slim/spans-at-borders ()
  (pm-test-run-on-file poly-slim-mode "slim.slim"
    (pm-map-over-spans
     (lambda ()
       (let* ((sbeg (nth 1 *span*))
              (send (nth 2 *span*))
              (range1 (pm-get-innermost-range sbeg))
              (range2 (pm-get-innermost-range send)))
         (should (eq sbeg (car range1)))
         (should (eq send (cdr range1)))
         (unless (eq send (point-max))
           (should (eq send (car range2)))))))))

(ert-deftest poly-slim/spans-at-narrowed-borders ()
  (pm-test-run-on-file poly-slim-mode "slim.slim"
    (pm-map-over-spans
     (lambda ()
       (pm-with-narrowed-to-span *span*
         (let* ((range1 (pm-get-innermost-range (point-min)))
                (range2 (pm-get-innermost-range (point-max))))
           (should (eq (car range1) (point-min)))
           (should (eq (cdr range1) (point-max)))
           (should (eq (car range2) (point-min)))
           (should (eq (cdr range2) (point-max)))))))))

(ert-deftest poly-slim/narrowed-spans ()
  (pm-test-run-on-file poly-slim-mode "slim.slim"
    (narrow-to-region 60 300)
    (let ((span (pm-innermost-span (point-min))))
      (should (eq (car span) 'head))
      (should (= (nth 1 span) 60))
      (should (= (nth 2 span) 66)))
    (let ((span (pm-innermost-span (point-max))))
      (should (eq (car span) 'body))
      (should (= (nth 1 span) 261))
      (should (= (nth 2 span) 300)))))

(ert-deftest poly-slim/spans-at-point-max ()
  (pm-test-run-on-file poly-slim-mode "slim.slim"
    (goto-char (point-max))
    (pm-switch-to-buffer)

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) 'body))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) 'head))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) 'nil))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) 'body))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))

    (let ((span (pm-get-innermost-span (point-max))))
      (should (eq (car span) 'head))
      (should (eq (nth 2 span) (point-max)))
      (delete-region (nth 1 span) (nth 2 span)))))
