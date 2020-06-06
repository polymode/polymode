
## Major Modes Guidelines

In order for major modes to work well with polimodes major modes should abide
the following rules:

  - Don't assume that there is no narrowing in place. If you retrieve
    text-properties or access a point in a buffer, make sure it's inside the
    accessible region of the buffer. Pay special attention to what happens at
    `point-max`.
  - Use `syntax-ppss` instead of `parse-partial-sexp`.
  - If your mode uses or defines some notion of a span or chunk, follow Emacs
    convention for text properties and consider spans as [left closed and right
    open) intervals.

## API

All API classes and methods are named with `pm-` prefix.

Buffer local objects:

   - `pm/polymode`
   - `pm/chunkmode` [!! don't rely on this one. Likely to change !!]

Generics:

   - `pm-initialize` 
   - `pm-get-buffer-create`
   - `pm-select-buffer`
   - `pm-get-span`
   - `pm-indent-line`
   - `pm-get-adjust-face`

Utilities:

  - Spans:
  
    - `pm-innermost-span`
    - `pm-map-over-spans`
    - `pm-narrow-to-span`
   
  - Buffers
  
    - `pm-set-buffer`
    - `pm-switch-to-buffer`

  - Others
  
    - `pm-fun-matcher`

## Initialization of polymodes

<!-- When called, `poly-XXX-mode` (created with `define-polymode`) clones -->
<!-- `pm-poly/XXX` object and calls `pm-initialize` generic on it. The actual -->
<!-- initialization depends on concrete type of the `pm-polymode` object but these -->
<!-- are the common steps: -->

<!--    1. assign the config object into local `pm/polymode` variable -->
<!--    2. clone the `pm-chunkmode` object specified by `:hostmode` slot of -->
<!--    `pm-polymode` -->
<!--    3. initialize hostmode by running the actual function in `:mode` slot of the -->
<!--    hostmode object. -->
<!--    4. store hostmode object into local `pm/chunkmode` variable -->
<!--    5. set local variable `pm/type` to `'host` -->
<!--    6. run `pm-polymode`'s `:init-functions` as normal hooks -->
<!--    7. run `pm--setup-buffer` which is common setup function used internally to -->
<!--       set `font-lock` and a range of other stuff -->
<!--    8. run `poly-XXX-mode-hook`. -->

<!-- Discovery of the spans is done by `pm-select-buffer` generic which is commonly -->
<!-- called first by `jit-lock`. `pm-select-buffer` fist checks if the corresponding -->
<!-- `pm-chunkmode` object (and associated indirect buffer) has been already -->
<!-- created. If so, `pm-select-buffer` simply selects that buffer. Otherwise, it -->
<!-- calls `pm-get-buffer-create` generic which, in turn, creates `pm-chunkmode` -->
<!-- object and the associated indirect buffer. -->

_TODO_

## Poly Lock

_TODO_

## Debugging Polymodes

You can visually inspect if the polymode does what you intended by activating
globalized pm-debug minor mode with <kbd>M-x pm-debug-mode</kbd>. When
`pm-debug-mode` is active the current span will be highlighted.

<img src="../img/debug.png"/>

Some useful commands defined in `pm-debug-mode-map`:

Shortcut | Description
---------|-------------
<kbd>M-n M-f</kbd> | Toggle font-locking (`pm-debug-toggle-fontification`)
<kbd>M-n M-h</kbd> | Map through all spans and briefly blink each span (`pm-debug-map-over-spans-and-highlight`)
<kbd>M-n M-i</kbd> | Highlight current span and display more info (`pm-debug-info-on-span`)
<kbd>M-n M-p</kbd> | Print values of relevant variables in current buffer (`pm-debug-relevant-variables`)
<kbd>M-n M-t i</kbd> | Toggle info messages (`pm-debug-toogle-info-message`)
<kbd>M-n M-t f</kbd> | Toggle whether font-lock should be performed (`pm-debug-toggle-fontification`)
<kbd>M-n M-t p</kbd> | Toggle post-command hook (`pm-debug-toggle-post-command`)
<kbd>M-n M-t c</kbd> | Toggle after-change hook (`pm-debug-toggle-after-change`)
<kbd>M-n M-t a</kbd> | Toggle all (`pm-debug-toggle-all`)
<kbd>M-n M-t t</kbd> | Toggle tracing (`pm-toggle-tracing`)
<kbd>M-n M-t v</kbd> | Toggle verbose messages from polymode (`pm-debug-toggle-verbose`)
<kbd>M-n M-f s</kbd> |  Fontify current span (useful for font-lock debugging when font-lock is off) (`pm-debug-fontify-current-span`)
<kbd>M-n M-f b</kbd> |  Fontify current buffer (`pm-debug-fontify-current-buffer`)

`pm-toggle-tracing` and `pm-trace` are particularly useful to debug syntax and font-lock calls.
