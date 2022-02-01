
## Global Customiation

For global customization you either set the slots of the root configuration
objects:

```el
   (oset poly-root-innermode :adjust-face ...)
```

Or, use the global polymode initialization hooks `polymode-init-inner-hook` and
`polymode-init-host-hook` and set the sots of the local `pm/chunkmode` object
after it was initialized:

```el
(defun my/inner-mode-setup ()
  (oset pm/chunkmode adjust-face my/inner-mode-face-props)
  ...)

(add-hook 'polymode-init-inner-hook my/inner-mode-face-setup)
```

## Specific Customization

Either set the slots of dedicated `inner`, `host` or `polymode` objects, or run
the initialization inside `poly-XYZ-mode-hook`, just like with standard Emacs
modes.

## Examples

### Fixed Pitch Innermode Faces

```el

(defvar my/inner-mode-face-props
  (list
   (list
    :background (poly-lock--adjusted-background 2)
    :height (face-attribute 'fixed-pitch :height)
    :family (face-attribute 'fixed-pitch :family)
    :font   (face-attribute 'fixed-pitch :font)
    :extend t)))


(oset-default pm-innermode :adjust-face my/inner-mode-props)

```


```el

(defvar my/inner-mode-face-props
  (list
   (list
    :background (poly-lock--adjusted-background 2)
    :height (face-attribute 'fixed-pitch :height)
    :family (face-attribute 'fixed-pitch :family)
    :font   (face-attribute 'fixed-pitch :font)
    :extend t)))

(oset-default pm-innermode :adjust-face my/inner-mode-props)

```

or it can be set in `pm-inner` 

```el
(defun my/inner-mode-face-setup ()
  (oset pm/chunkmode adjust-face my/inner-mode-face-props))

(add-hook 'polymode-init-inner-hook my/inner-mode-face-setup)

```


```el
;; for org-mode only setup
(oset poly-org-innermode :adjust-face )

;; for global setup
;; (add-hook 'polymode-init-inner-hook my/inner-mode-face-setup)

```
