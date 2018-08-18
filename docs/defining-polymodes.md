
Defining polymodes always starts by defining one host and at least one inner
chunkmode.

## Hosts Modes

Here is an example of markdown host mode:

```el

(defcustom pm-host/markdown
  (pm-host-chunkmode "markdown"
                     :mode 'markdown-mode)
  "Markdown host chunkmode"
  :group 'hostmodes
  :type 'object)

```

Here we call `pm-host-chunkmode`, the constructor of host chunkmodes, with the
`:mode` parameter which should be the name of a valid emacs major mode function.
In this case `markdown-mode`.

_Note: First argument in constructors is the name of the object. This name is
used in various contexts for object identification_.

## Inner Modes

Polymode defines two built-in types of inner chunkmodes, simple
`pm-inner-chunkmode` which allows for one pre-specified mode and
`pm-inner-auto-chunkmode` which detects the type of the chunk dynamically. 

This is an example of two markdown auto innermodes, one for fenced code and the
other for inner code:

```el

(defcustom  pm-inner/markdown-fenced-code
  (pm-inner-auto-chunkmode "markdown-fenced-code"
                           :head-matcher "^[ \t]*```[{ \t]*\\w.*$"
                           :tail-matcher "^[ \t]*```[ \t]*$"
                           :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1))
  "Markdown fenced code block."
  :group 'innermodes
  :type 'object)

(defcustom  pm-inner/markdown-inline-code
  (pm-inner-auto-chunkmode "markdown-inline-code"
                           :head-matcher (cons "[^`]\\(`{?[[:alpha:]+-]+\\)[ \t]" 1)
                           :tail-matcher (cons "[^`]\\(`\\)[^`]" 1)
                           :mode-matcher (cons "`[ \t]*{?\\(?:lang *= *\\)?\\([[:alpha:]+-]+\\)" 1)
                           :head-mode 'host
                           :tail-mode 'host)
  "Markdown inline code."
  :group 'innermodes
  :type 'object)

```

Here `:head-matcher` and `:tail-matcher` are regular expressions defining the
head and the tail of the chunks. The `:mode-matcher` tells polymode how to
retrieve the major mode from the head of the chunk. Each of the three marchers
can be a regexp a cons of the form (REGEXP . SUBMATCH) or a function which
should return the name of the mode.

`:head-mode` and `:tail-mode` specify the major mode which should be used for
head and tail respectively. If it's a special symbol `'host` host mode is used
instead, if `'body` the chunks body mode. It defaults to `poly-head-tail-mode`
which is at the moment a very basic prog mode with no special powers.

## Polymodes

Finally, use the host and inner modes defined earlier to define the
`poly-markdown-mode` polymode:

```el
(define-polymode poly-markdown-mode
  :hostmode 'pm-host/markdown
  :innermodes '(pm-inner/markdown-fenced-code
                pm-inner/markdown-inline-code))
```

`define-polymode` is similar to the standard Emacs utilities
`define-derived-mode` and `define-minor-mode`. It accepts several optional
arguments - `PARENT` polymode to be derived from, `DOC` string and `BODY` which
is executed in host and every indirect buffer during the installation of the
chunkmodes. `BODY` can be preceded by key-value pairs further refining the
configuration. By far the most common keys are `:hostmode` specify the name of
the `pm-host-chunkmode` object and `:innermodes` which is a list of names of
`pm-inner-chunkmode` objects. See the documentation of `define-polymode` for
further details.

On a technical level polymode functions are more similar to minor-modes. In fact
every `poly-NAME-mode` chunkmode buffer (base or indirect) will have
`poly-NAME-mode` minor mode activated, thus acting like a UI "glue" between
different chunks.

When defining `poly-NAME-mode` 3 objects are created in the background:

  - `poly-NAME-mode` function which when can be used as major or minor mode
  - `poly-NAME-mode-map` keymap which inherits from `PARENT`'s polymode keymap
    or `polymode-mode-map` if no `PARENT`.
  - `pm-poly/NAME` configuration object derived (cloned) from the `PARENT`'s
    configuration object if `PARENT` is non-nil.
    
`poly-NAME-mode` will also run `poly-NAME-mode-hook` (and all parents' hooks) in
every chunkmode buffer after the initialization of the chunkmode has been
completed.


`PARENT` argument can also be a configuration object. When creation of polymodes
for parent objects has little or no sense. For example `pm-poly/latex` is as
parent of `pm-poly/noweb` but having a dedicated `poly-latex-mode` polymode has
little sense.

```el
(defcustom pm-poly/latex
  (pm-polymode "latex" :hostmode 'pm-host/latex)
  "LaTeX typical configuration."
  :group 'polymodes
  :type 'object)

(define-polymode poly-noweb-mode pm-poly/latex
  :innermodes '(pm-inner/noweb)
  :keylist '(("<" . poly-noweb-electric-<)))

```

When name of `PARENT` matches the name of the polymode, `define-polymode` uses
`PARENT` directly as the configuration object (i.e. no clone or `defcustom` is
performed). Therefore, the above definition of `poly-noweb-mode` is essentially
equivalent to the following:

```el
(defcustom pm-poly/noweb
  (clone pm-poly/latex "noweb"
         :innermodes '(pm-inner/noweb)
         :keylist '(("<" . poly-noweb-electric-<)))
  "Noweb polymode configuration."
  :group 'polymodes
  :type 'object)

(define-polymode poly-noweb-mode pm-poly/noweb)
```

