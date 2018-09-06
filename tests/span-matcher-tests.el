(require 'polymode-test-utils)

(defvar markdown-inline-head-fun-matcher (pm-fun-matcher (cons "[^`]\\(`{?[[:alpha:]+-]+\\)[ \t]" 1)))
(defvar markdown-inline-tail-fun-matcher (pm-fun-matcher (cons "[^`]\\(`\\)[^`]" 1)))
(defun markdown-inline-test-fun-matcher ()
  (pm--span-at-point
   markdown-inline-head-fun-matcher
   markdown-inline-tail-fun-matcher))

(ert-deftest span-matcher/markdown-inline ()
  (pm-test-matcher
   "1. Lists
--------

Unordered lists:

- This is a `el (defvar bullet-point 'bullet-point')`.
- This is a `ada 'Sub bullet point'`.
- This is `python ['*' + x + '*' for x in ['another', 'bullet', 'point']]`

2. Ordered Lists
----------------
"
   '((1 . (nil 1 50))
     (50 . (head 50 53))
     (53 . (body 53 90))
     (90 . (tail 90 91))
     (91 . (nil 91 105))
     (105 . (head 105 109))
     (109 . (body 109 128))
     (128 . (tail 128 129))
     (129 . (nil 129 141))
     (141 . (head 141 148))
     (148 . (body 148 204))
     (204 . (tail 204 205))
     (205 . (nil 205 241)))
   #'markdown-inline-test-fun-matcher))

(ert-deftest span-matcher/markdown-inline-extremes ()
  (pm-test-matcher
   " `el (defvar bullet-point 'bullet-point')`
`ada 'Sub bullet point'` `aba 'Sub bullet point'`
`python ['*' + x + '*' for x in ['another', 'bullet', 'point']]`
"
   '((1 . (nil 1 2))
     (2 . (head 2 5))
     (5 . (body 5 42))
     (42 . (tail 42 43))
     (43 . (nil 43 44))
     (44 . (head 44 48))
     (48 . (body 48 67))
     (67 . (tail 67 68))
     (68 . (nil 68 69))
     (69 . (head 69 73))
     (73 . (body 73 92))
     (92 . (tail 92 93))
     (93 . (nil 93 94))
     (94 . (head 94 101))
     (101 . (body 101 157))
     (157 . (tail 157 158))
     (158 . (nil 158 159))
     )
   #'markdown-inline-test-fun-matcher))

(ert-deftest span-matcher/markdown-inline-incomplete ()
  (pm-test-matcher
   " some text `el "
   '((1 . (nil 1 12))
     (12 . (head 12 15))
     (15 . (body 15 16)))
   #'markdown-inline-test-fun-matcher))

(ert-deftest span-matcher/markdown-fenced-code ()
  (pm-test-matcher
   "
# Fenced Code

```foo
1 + 1
```

```{boo bar +- baz;;  }
## [1] 2

```

   ```bar
     0.4 - 0.7 + 0.3  # what? it is not zero!
   ```

```foo-bar-baz qwer asdf uiop
  ## [1] 5.551e-17
    ```

some ```extra words''' here
"
   '((1 . (nil 1 17))
     (17 . (head 17 23))
     (23 . (body 23 30))
     (30 . (tail 30 33))
     (33 . (nil 33 35))
     (35 . (head 35 58))
     (58 . (body 58 69))
     (69 . (tail 69 72))
     (72 . (nil 72 74))
     (74 . (head 74 83))
     (83 . (body 83 130))
     (130 . (tail 130 136))
     (136 . (nil 136 138))
     (138 . (head 138 167))
     (167 . (body 167 187))
     (187 . (tail 187 194))
     (194 . (nil 194 224)))
   (lambda ()
     (pm--span-at-point
      (pm-fun-matcher (cons "^[ \t]*```[{ \t]*\\w.*$" 0))
      (pm-fun-matcher (cons "^[ \t]*```[ \t]*$" 0))))))

(ert-deftest span-matcher/inner-submatch-extremes ()
  (pm-test-matcher
   "--`first-- span --`--
--`el-- (defvar x 1)--`----`ada-- 'Sub bullet point'--`--.
--`ba-- ba -- ba
ba ba--`---baz---`last-- span --`--"
   '((1 . (nil 1 3))
     (3 . (head 3 9))
     (9 . (body 9 19))
     (19 . (tail 19 20))
     (20 . (nil 20 25))
     (25 . (head 25 28))
     (28 . (body 28 45))
     (45 . (tail 45 46))
     (46 . (nil 46 50))
     (50 . (head 50 54))
     (54 . (body 54 77))
     (77 . (tail 77 78))
     (78 . (nil 78 84))
     (84 . (head 84 87))
     (87 . (body 87 106))
     (106 . (tail 106 107))
     (107 . (nil 107 116))
     (116 . (head 116 121))
     (121 . (body 121 131))
     (131 . (tail 131 132))
     (132 . (nil 132 134)))
   (lambda ()
     (pm--span-at-point
      (pm-fun-matcher (cons "--\\(`[[:alpha:]]+\\)--" 1))
      (pm-fun-matcher (cons "--\\(`\\)--" 1))))))

(ert-deftest span-matcher/inner-match-extremes ()
  (pm-test-matcher
   "<<span0>> sfds <<span 1>><<span2>><<"
   '((1 . (head 1 3))
     (3 . (body 3 8))
     (8 . (tail 8 10))
     (10 . (nil 10 16))
     (16 . (head 16 18))
     (18 . (body 18 24))
     (24 . (tail 24 26))
     (26 . (head 26 28))
     (28 . (body 28 33))
     (33 . (tail 33 35))
     (35 . (head 35 37)))
   (lambda ()
     (pm--span-at-point
      (pm-fun-matcher (cons "<<" 0))
      (pm-fun-matcher (cons ">>" 0))))))

(ert-deftest span-matcher/inner-submatch ()
  (pm-test-matcher
   "--`first-- span --`--
Some text:

- This is a --`el-- (defvar bullet-point 'bullet-point')--`--.
- This is a --`ada 'Sub bullet point'`.

foo
---   bar -------------

 - This is --`ba-- ba -- ba
ba
ba ba--`---

baz
--`last-- span --`--"
   '((1 . (nil 1 3))
     (3 . (head 3 9))
     (9 . (body 9 19))
     (19 . (tail 19 20))
     (20 . (nil 20 49))
     (49 . (head 49 52))
     (52 . (body 52 93))
     (93 . (tail 93 94))
     (94 . (nil 94 181))
     (181 . (head 181 184))
     (184 . (body 184 206))
     (206 . (tail 206 207))
     (207 . (nil 207 218))
     (218 . (head 218 223))
     (223 . (body 223 233))
     (233 . (tail 233 234))
     (234 . (nil 234 236)))
   (lambda ()
     (pm--span-at-point
      (pm-fun-matcher (cons "--\\(`[[:alpha:]]+\\)--" 1))
      (pm-fun-matcher (cons "--\\(`\\)--" 1))))))


(ert-deftest span-matcher/markdown-latex ()
  (pm-test-matcher
   "Some text
$$%
\\int_a^b f(x) dx
$$
Some text
"
   '((1 . (nil 1 11))
     (11 . (head 11 13))
     (13 . (body 13 32))
     (32 . (tail 32 34))
     (34 . (nil 34 45)))
   (lambda ()
     (pm--span-at-point
      (pm-fun-matcher (cons "^[ \t]*\\(\\$\\$\\)." 1))
      (pm-fun-matcher (cons "\\(\\$\\$\\)$" 1))))))

(defun tt ()
  (interactive)
  (message
   "%S"
   (pm--span-at-point
    (pm-fun-matcher (cons "[^$]\\(\\$\\)[^ $\t[:digit:]]" 1))
    (pm-fun-matcher (cons "[^ $\t]\\(\\$\\)[^$]" 1)))
   ;; (markdown-inline-test-fun-matcher)
   ;; (pm--span-at-point
   ;;  (pm-fun-matcher (cons "^[ \t]*\\(\\$\\$\\)." 1))
   ;;  (pm-fun-matcher (cons "\\(\\$\\$\\)$" 1)))
   ))


;;; regexp look up benchmarks before pm--span-at-point-reg-reg was removed

;; (setq tt-head-matcher (pm-fun-matcher markdown-inline-reg-head-matcher))
;; (setq tt-tail-matcher (pm-fun-matcher markdown-inline-reg-tail-matcher))

;; ;; Conclusion marginal benefits are not worth keeping a separate reg-reg version.
;; ;; Measured with (benchmark-run 10 (tt-bench-reg)) on markdown.md
;; (defun tt-bench-reg ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (not (eobp))
;;       ;; 12.84 sec (closure creation each iteration)
;;       (pm--span-at-span-point
;;        (pm-fun-matcher markdown-inline-reg-head-matcher)
;;        (pm-fun-matcher markdown-inline-reg-tail-matcher))
;;       ;; 12.70 sec (cached closures)
;;       ;; (pm--span-at-span-point tt-head-matcher tt-tail-matcher)
;;       ;; 12.46 sec / pure reg-reg matcher before removal /
;;       ;; (markdown-inline-test-reg-matcher)
;;       (forward-char 1))))

;; (profiler-start 'cpu)
;; (profiler-report)
;; (profiler-stop)
