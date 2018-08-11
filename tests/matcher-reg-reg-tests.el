
(require 'polymode-test)

(ert-deftest matcher-reg-reg/markdown-inline ()
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
   (lambda ()
     (pm--span-at-point-reg-reg
      (cons "[^`]\\(`{?[[:alpha:]+-]+\\)[ \t]" 1)
      (cons "[^`]\\(`\\)[^`]" 1)))))

(ert-deftest matcher-reg-reg/markdown-fenced-code ()
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
     (pm--span-at-point-reg-reg
      (cons "^[ \t]*```[{ \t]*\\w.*$" 0)
      (cons "^[ \t]*```[ \t]*$" 0)))))

(ert-deftest matcher-reg-reg/inner-submatch ()
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
     (pm--span-at-point-reg-reg
      (cons "--\\(`[[:alpha:]]+\\)--" 1)
      (cons "--\\(`\\)--" 1)))))
