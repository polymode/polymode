
Defining polymodes always starts with defining (or reusing) a hostmode and one
or more innermodes.

## Hosts Modes

Here is how the markdown hostmode is defined:


```el
(define-hostmode poly-markdown-hostmode
  :mode 'markdown-mode)
```

`define-hostmode` is a macro which, in this case, simply defines an object
`poly-markdown-hostmode` of class `pm-host-chunkmode`. Keyword arguments are
slots, of which the most important being `:mode` naming the emacs' major-mode
which should be installed in host chunks.

The optional second positional argument to `define-hostmode` is the parent
hostmode. When provided the entire configuration of the parent is inherited by
the child. A wide range of hostmodes is already defined in the
[polymode-base.el](https://github.com/polymode/polymode/blob/master/polymode-base.el)
file. Please re-use those in your own polymodes.

## Inner Modes

Polymode defines two built-in types of innermodes - `pm-inner-chunkmode` which
allows for one pre-specified in advance mode, and `pm-inner-auto-chunkmode`
which detects the major mode of the body span on-the-fly.

Here is an example of the YAML metadata innermode for markdown

```el
(define-innermode poly-markdown-yaml-metadata-innermode
  :mode 'yaml-mode
  :head-matcher "\`[ \t\n]*---\n"
  :tail-matcher "^---\n"
  :head-mode 'host
  :tail-mode 'host)
```

and a fenced-code auto innermode

```el
(define-auto-innermode poly-markdown-fenced-code-innermode
  :head-matcher (cons "^[ \t]*\\(```{?[[:alpha:]].*\n\\)" 1)
  :tail-matcher (cons "^[ \t]*\\(```\\)[ \t]*$" 1)
  :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1)
  :head-mode 'host
  :tail-mode 'host)
```

In both of the examples `:head-matcher` and `:tail-matcher` are regular
expressions patterns used to search for heads and tails of inner code
chunks. The `:mode-matcher` tells polymode how to retrieve the major mode from
the head of the chunk. Each of the three marchers can be a regexp, a cons of the
form (REGEXP . SUBMATCH) or a function which should return the name of the mode.

`:head-mode` and `:tail-mode` specify the major mode which should be used for
head and tail respectively. Special symbol `'host` means that the host mode is
used for head or tail, if `'body` - the mode of the chunk's body. Head and tail
modes default to `poly-head-tail-mode` which is a very basic prog mode with no
special powers.

Buffer local `polymode-default-inner-mode` can be used to specify the default
mode for body spans either for anonymous (aka mode-less) chunks or when the
major mode cannot be identified. When this variable is nil (the default) either
a host mode or a special `poly-fallback-mode` is installed. Which mode is
installed depends on the value of `:mode` slot of the innermode configuration
object.

## Polymodes

Finally, use the host and inner modes defined earlier to define the
`poly-markdown-mode` polymode:

```el
(define-polymode poly-markdown-mode
  :hostmode 'pm-host/markdown
  :innermodes '(poly-markdown-yaml-metadata-innermode
                poly-markdown-fenced-code-innermode))
```

`define-polymode` is similar to the standard Emacs utilities
`define-derived-mode` and `define-minor-mode`. It accepts several optional
arguments - `PARENT` polymode to be derived from, `DOC` string and `BODY` which
is executed in host and every indirect buffer during the installation of the
chunkmodes. `BODY` can be preceded by key-value pairs further refining the
configuration. By far the most common keys are `:hostmode` specify the name
(symbol) of the `pm-host-chunkmode` object and `:innermodes` which is a list of
names of `pm-inner-chunkmode` objects. See the documentation of
`define-polymode` for further details.

Most polymodes are designed to be used as major modes (i.e. in `auto-mode-alist`
or buffer-local `mode:` cookie). Some polymodes (those with host's `:mode` slot
set to `nil`) are explicitly designed to be used as minor modes.

Internally, all polymode functions are in fact minor-modes. Thus, you can, for
example, toggle them with `M-x poly-XYZ-mode`. Every polymode buffer (base or
indirect) will have this `poly-XYZ-mode` minor mode activated in order to
provide the "glue" interface between different chunks. If you want your commands
to be available in all chunks bind them in `poly-XYZ-mode-map` or directly in
`poly-mode-map` which is the root map of all polymode maps.

A call to `define-polymode` creates 3 objects in the background:

  - `poly-XYZ-mode` function which when can be used as major or minor mode.
  - `poly-XYZ-mode-map` keymap which inherits from `PARENT`'s polymode keymap
    or `polymode-mode-map` if no `PARENT` has been provided.
  - `poly-XYZ-polymode` configuration object derived (cloned) from `pm-polymode`
    object or the `PARENT`'s configuration object if `PARENT` has been provided.
    
`poly-XYZ-mode` will also run `poly-XYZ-mode-hook` (and all parents' hooks) in
every chunkmode buffer after the initialization of the chunkmode has been
completed.

### Config Objects as Parents 

On some occasions the creation of polymode functions for parent objects has
little or no sense. For example `poly-latex-root-polymode` is a parent of the
`poly-noweb-polymode` but having a dedicated `poly-latex-mode` polymode has not
much sense. For such cases `PARENT` argument of `define-polymode` can also be a
configuration object.

```el
(defvar poly-latex-root-polymode
  (pm-polymode :name "latex" :hostmode 'poly-latex-hostmode)
  "LaTeX root configuration.")

(define-polymode poly-noweb-mode poly-latex-root-polymode
  :innermodes '(pm-inner/noweb)
  :keylist '(("<" . poly-noweb-electric-<)))
```

In a special case when name of `PARENT` matches the name of the polymode,
`PARENT` is used directly as the configuration object (no clone or `defcustom`
is performed, as that wouldn't make sense). Therefore, the above definition of
`poly-noweb-mode` is equivalent to the following definition with an explicit
`poly-noweb-polymode` config:

```el
(defvar poly-noweb-polymode
  (clone poly-latex-root-polymode
         :name "noweb"
         :innermodes '(poly-noweb-innermode)
         :keylist '(("<" . poly-noweb-electric-<)))
  "Noweb polymode configuration.")

(define-polymode poly-noweb-mode poly-noweb-polymode)
```

