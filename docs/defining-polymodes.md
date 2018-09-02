
Defining polymodes always starts by defining one host chunkmode and at least one
inner chunkmode.

## Hosts Modes

Here is an example of markdown host mode:


```el

(defcustom pm-host/markdown
  (pm-host-chunkmode :name "markdown"
                     :mode 'markdown-mode)
  "Markdown host chunkmode"
  :group 'poly-hostmodes
  :type 'object)

```

Here we call the constructor of host chunkmode objects `pm-host-chunkmode` with
the `:mode` parameter which should be a name (symbol) of a valid emacs major
mode function. In this case `markdown-mode`. 

The`:name` slot is used in various contexts for object identification and
information messages.

## Inner Modes

Polymode defines two built-in types of inner chunkmodes - `pm-inner-chunkmode`
which allows for one pre-specified mode and `pm-inner-auto-chunkmode` which
detects the type of the body span dynamically.

This is an example of two markdown auto innermodes:

```el

(defcustom  pm-inner/markdown-fenced-code
  (pm-inner-auto-chunkmode :name "markdown-fenced-code"
                           :head-matcher "^[ \t]*```[{ \t]*\\w.*$"
                           :tail-matcher "^[ \t]*```[ \t]*$"
                           :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1))
  "Markdown fenced code block."
  :group 'poly-innermodes
  :type 'object)

(defcustom  pm-inner/markdown-inline-code
  (pm-inner-auto-chunkmode :name "markdown-inline-code"
                           :head-matcher (cons "[^`]\\(`{?[[:alpha:]+-]+\\)[ \t]" 1)
                           :tail-matcher (cons "[^`]\\(`\\)[^`]" 1)
                           :mode-matcher (cons "`[ \t]*{?\\(?:lang *= *\\)?\\([[:alpha:]+-]+\\)" 1)
                           :head-mode 'host
                           :tail-mode 'host)
  "Markdown inline code."
  :group 'poly-innermodes
  :type 'object)

```

Here `:head-matcher` and `:tail-matcher` are regular expressions used to search
for heads and tails of inner code chunks. The `:mode-matcher` tells polymode how
to retrieve the major mode from the head of the chunk. Each of the three
marchers can be a regexp a cons of the form (REGEXP . SUBMATCH) or a function
which should return the name of the mode.

`:head-mode` and `:tail-mode` specify the major mode which should be used for
head and tail respectively. Special symbol `'host` means that the host mode is
used for head or tail; if `'body` the chunk's body mode. Head and tail modes
default to `poly-head-tail-mode` which is a very basic prog mode with no special
powers.

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

At the implementation level polymode functions are more similar to minor-modes
than major modes. In fact every `poly-NAME-mode` chunkmode buffer (base or
indirect) will have `poly-NAME-mode` minor mode activated in order to provide
the "glue" interface between different chunks. If you want some commands to be
available in all chunks bind them in `poly-NAME-mode-map` or directly in
`poly-mode-map` which is the root map of all polymode maps.

A call to `define-polymode` creates 3 objects in the background:

  - `poly-NAME-mode` function which when can be used as major or minor mode
  - `poly-NAME-mode-map` keymap which inherits from `PARENT`'s polymode keymap
    or `polymode-mode-map` if no `PARENT` has been provided
  - `pm-poly/NAME` configuration object derived (cloned) from `pm-polymode`
    object or the `PARENT`'s configuration object if `PARENT` has been provided
    
`poly-NAME-mode` will also run `poly-NAME-mode-hook` (and all parents' hooks) in
every chunkmode buffer after the initialization of the chunkmode has been
completed.

### Config Objects as Parents 

`PARENT` argument can also be a configuration object. Sometimes creating of
polymodes for parent objects has little or no sense. For example `pm-poly/latex`
is as parent of `pm-poly/noweb` but having a dedicated `poly-latex-mode`
polymode has no sense.

```el
(defcustom pm-poly/latex
  (pm-polymode :name "latex"
               :hostmode 'pm-host/latex)
  "LaTeX typical configuration."
  :group 'polymodes
  :type 'object)

(define-polymode poly-noweb-mode pm-poly/latex
  :innermodes '(pm-inner/noweb)
  :keylist '(("<" . poly-noweb-electric-<)))

```

In a special case when name of `PARENT` matches the name of the polymode,
`PARENT` is used directly as the configuration object (no clone or `defcustom`
is performed; it wouldn't make sense). Therefore, the above definition of
`poly-noweb-mode` is equivalent to the following definition with an explicit
`pm-poly/noweb` config:

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

