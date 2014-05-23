# Developing with Polymode

Polymode doesn't keep its modes in a single emacs buffer but in several indirect
buffers, as many as different modes are there in a file. Consequently, polymode
is as fast as switching emacs buffers because it never re-installs major modes
like other multi-modes do. I am very much indebted to Dave Love's
[multi-mode.el](http://www.loveshack.ukfsn.org/emacs/multi-mode.el) for this
awesome idea.

- [Glossary of Terms](#glossary-of-terms)
- [Class Hierarchy](#class-hierarchy)
  - [Polymodes](#polymodes)
  - [Chunkmodes](#chunkmodes)
- [Defining New Polymodes](#defining-new-polymodes)
  - [One Predefined Innermode](#one-predefined-innermodes)
  - [Multiple Predefined Innermodes](#multiple-predefined-innermodes)
  - [Multiple Automatically Detected Innermodes](#multiple-automatically-detected-innermodes)
- [Defining Literate Programming Backends](#defining-backends-weavers-exporters-and-tanglers)
  - [Weavers](#weavers)
  - [Exporters](#exporters)
  - [Tanglers](#tanglers)
- [Internals](#internals)
  - [API](#api)
  - [Initialization of polymodes](#initialization-of-polymodes)


## Glossary of Terms

Assume the following `org-mode` file:

```org
* emacs lisp code block

#+begin_src emacs-lisp :var tbl='()
 (defun all-to-string (tbl)
    (if (listp tbl)
        (mapcar #'all-to-string tbl)
      (if (stringp tbl)
          tbl
        (format "%s" tbl))))
  (all-to-string tbl)
#+end_src

```

 - **span** is a homogeneous in terms of syntactic content fragment of text. In
   the `org-mode` example the org span starts at the beginning of the file till,
   but not including, `#+begin_src`. Header span is `#+begin_src emacs-lisp :var
   tbl='()`. The emacs lisp code span is next, followed by `#+end_src` tail
   span.
 - **chunk** is a well delimited fragment of text that consists of one or more
   spans. Most common types of chunks are bchunk (plain body chunk) and hbtchunk
   (chunk composed of head, body and tail spans). In our example, org-mode span
   and org-mode chunk are the same thing and the region that starts with
   `#+begin_src` and ends with `#+end_src` is an org emacs-lisp chunk.
 - **polymode** has three concurrent meanings which we will disambiguate from
   the context:
   1. Like emacs modes, polymodes represent an _abstract idea_ of a collection
      of related functionality that is available in emacs buffers.
   2. Like emacs modes, polymodes are _functions_ that install a bunch of
      functionality into emacs buffer. You can use polymodes just as any other
      emacs major or minor mode.
   3. Polymodes are _objects_ of `pm-polymode` class. The functionality of each
      polymode is completely characterized by this object and the methods that
      act on it. During initialization this object is cloned and its copy is
      stored in a buffer-local variable `pm/polymode`. There are several types
      of polymode objects. See [hierarchy](#class-hierarchy) below.
 - **chunkmode** refers to one of the following:
   1. An abstract idea of the functionality available in chunks of the same type
      (e.g. org chunks, emacs-lisp chunks).
   2. Emacs mode function (e.g. `org-mode`), or a collection of such functions
      (e.g. `fundamental-mode` for header/tail and `emacs-lisp-mode` for the
      chunk's body) that instantiate the functionality into the corresponding
      modes.
   3. Object of `pm-chunkmode` class. This object represents the behavior of the
      chunkmode and is stored in a buffer-local variable `pm/chunkmode`. There
      are several types of chunkmode objects. See [hierarchy](#class-hierarchy)
      below.
 - **hostmodes** and **innermodes** Chunkmodes could be classified into host and
   inner chunkmodes (hostmodes and innermodes in short). In the above example
   org chunkmode is a hostmode and emacs-lisp chunkmode is an innermode.


It is easy to think of the chunkmodes as inter-weaved threads. Host chunkmode is
a stretched canvas. Each inner chunkmode is a thread weaved into the
hostmode. Visible fragments of the each thread are chunks.

In light of the above metaphor, it is worth emphasizing the distinctions between
`chunks` and `chunkmodes`. Chunks are fragments of text and there might be
multiple chunks of the same type in the buffer. In contrast, there is only one
chunkmode of some specific type and multiple chunks of this type "share" this
chunkmode.

 
## Class Hierarchy

Polymode package uses `eieio` to represent its objects. The root class for all
polymode classes is `eieio-instance-inheritor` which provides prototype based
inheritance (in addition to class based). This means that objects instantiated
from polymode classes can be cloned in order to dynamically create a hierarchy
of customizable objects. There are a bunch of such objects already defined, you
can investigate those in `polymodes`, `hostmodes`, `innermodes` customization
groups.

As polymode uses indirect buffers to implement the multi-mode, storing mode
functionality in objects (in contrast to buffer local variables) is very
convenient strategy for moving stuff around.

Current polymode class hierarchy:

```
  +--eieio-instance-inheritor
  |    +--pm-root
  |         +--pm-polymode
  |         |    +--pm-polymode-multi
  |         |    |    +--pm-polymode-multi-auto
  |         |    +--pm-polymode-one
  |         |
  |         +--pm-chunkmode
  |         |    +--pm-hbtchunkmode
  |         |    |    +--pm-hbtchunkmode-auto
  |         |    +--pm-bchunkmode
  |         |
  |         +--pm-weaver
  |         |    +--pm-shell-weaver
  |         |    +--pm-callback-weaver
  |         +--pm-exporter
  |              +--pm-shell-exporter
  |              +--pm-callback-exporter

```

*Using Help with EIEIO:* Each `eieio` class has a corresponding constructor
whose docstring contains a complete description of the class. In emacs 24.4 or
higher you can use `C-h f pm-foo RET` to inspect the documentation of the
class. Alternatively either use `M-x describe-class pm-foo` or lookup the class
definition directly in [polymode-classes.el](polymode-classes.el).


### Polymodes

As noted earlier, each polymode is a function that walks and quacks like
standard emacs major mode. Hence, things like `poly-XXX-mode-map` and
`poly-XXX-mode-hook` work just as expected. Plymode functions are defined with
`define-polymode` and can be used in place of emacs standard major or minor
modes.

Each polymode is represented by a customizable `pm-polymode` object which fully
characterizes its behavior. During the initialization this config object is
cloned and installed in every new buffer.

The most important slot of root config class `pm-polymode` is:

- `:hostmode` - name of the chunkmode object (typicaly of class `pm-bchunkmode`,
  see [Chunkmodes](#chunkmodes)).

Currently there are three subclasses of `pm-polymode`:

- `pm-polymode-one` - used for polymdoes with only one predefined innermode. It
  extends `pm-polymode` with one slot - `:innermode` - which is a name of the
  inner chunkmode (typically objects of class `pm-hbtchunkmode`).
- `pm-polymode-multi` - used for polymodes with multiple predefined inner
  modes. It extends `pm-polymode` with `:innermodes` list that contains names of
  predefined `pm-hbtchunkmode` objects.
- `pm-polymode-multi-auto` - used for polymodes with multiple dynamically
  discoverable chunkmodes. It extends `pm-polymode-multi` with `:auto-innermode`
  slot (typically an object of class `pm-hbtchunkmode-auto`).


### Chunkmodes

Most important user visible slots of the root class `pm-chunkmode` are:

- `:mode` - symbol of corresponding emacs plain mode (e.g. `html-mode`,
`latex-mode` etc)
- `:indent-offset`, `:font-lock-narrow`, `:adjust-face`etc - configuration options.
   
Currently, there are three sub classes of `pm-chunkmode`:

1. `pm-bchunkmode` - represents the mode of plain body chunks
   (bchunks). These objects are commonly used to represent functionality in
   host chunks and are instances of `pm-bchunkmode`. Currently it doesn't
   add any new slots to its parent class `pm-chunkmode`.

2. `hbtchunkmode` - represents the mode of composite head-body-tail
   chunks. These objects are commonly used to represent the functionality of
   the innermost chunks of the buffer. `pm-hbtchunkmode` extends
   `pm-chunkmode` with additional slots, most importantly:
    * `head-mode` and `tail-mode`: names of emacs-modes for header/tail of the
      chunk
    * `head-reg` and `tail-reg`: regular expressions or functions to detect the
      header/tail

3. `pm-hbtchunkmode-auto` - represents chunkmodes for which the mode type is not
   predefined and must be computed at runtime. This class extends
   `pm-hbtchunkmode` with `retriver-regexp`, `retriver-num` and
   `retriver-function` which can be used to retrive the mode name from the
   header of the inner chunk.


## Defining New Polymodes

In order to define a new polymode `poly-cool-mode` you first have to define or
clone a chunkmode object to represent the hostmode, and one or more chunkmodes
to represent innermodes. Then define the polymode object `pm-poly/cool` pointing
to previously defined host and inner chunkmodes.

There are a lot of polymodes, hostmodes and innermodes already defined. Please
reuse those whenever possible.

### One Predefined Innermode

This is a simplified version of `poly-noweb-mode` from
[poly-noweb.el](poly-noweb.el). First define the latex hostmode:

```lisp
(defcustom pm-host/latex
  (pm-bchunkmode "latex" :mode 'latex-mode)
  "Latex host chunkmode"
  :group 'hostmodes
  :type 'object)
```

Then define the noweb innermode:

```lisp
(defcustom  pm-inner/noweb
  (pm-hbtchunkmode "noweb"
                   :head-reg  "<<\\(.*\\)>>="
                   :tail-reg    "\\(@ +%def .*\\)$\\|\\(@[ \n]\\)")
  "Noweb typical chunk."
  :group 'innermodes
  :type 'object)
```

Finally, define the `pm-polymode` object and the coresponding polymode function:

```lisp
(defcustom pm-poly/noweb
  (pm-polymode-one "noweb"
                   :hostmode 'pm-host/latex
                   :innermode 'pm-inner/noweb)
  "Noweb typical polymode."
  :group 'polymodes
  :type 'object)

(define-polymode poly-noweb-mode pm-poly/noweb)
```

The hostmode `pm-host/latex` from above is already defined in
[poly-base.el](poly-base.el), so you need not have declared it.

Now, let's assume you want a more specialized noweb mode, say `noweb` with `R`
chunks. Instead of declaring root hostmodes and innermodes again you should
clone existing noweb root object. This is how it is done (from
[poly-R.el](poly-R.el)):

```lisp
(defcustom pm-inner/noweb+R
  (clone pm-inner/noweb :mode 'R-mode)
  "Noweb innermode for R"
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/noweb+R
  (clone pm-poly/noweb :innermode 'pm-inner/noweb+R)
  "Noweb polymode for R"
  :group 'polymodes
  :type 'object)

(define-polymode poly-noweb+r-mode pm-poly/noweb+R :lighter " PM-Rnw")
```

That's it. You simply had to define new innermode and polymode by cloning from
previously defined objects and adjusting `:mode` and `:innermode` slots
respectively.

### Multiple Predefined Innermodes

No examples yet. Web-mode would probably qualify.

### Multiple Automatically Detected Innermodes

This is an example of markdown polymode (from [poly-markdown.el](poly-markdown.el)).

```lisp
;; 1. Define hostmode object
(defcustom pm-host/markdown
  (pm-bchunkmode "Markdown" :mode 'markdown-mode)
  "Markdown host chunkmode"
  :group 'hostmodes
  :type 'object)


;; 2. Define innermode object
(defcustom  pm-inner/markdown
  (pm-hbtchunkmode-auto "markdown"
                     :head-reg "^[ \t]*```[{ \t]*\\w.*$"
                     :tail-reg "^[ \t]*```[ \t]*$"
                     :retriever-regexp "```[ \t]*{?\\(\\(\\w\\|\\s_\\)*\\)"
                     :font-lock-narrow t)
  "Markdown typical chunk."
  :group 'innermodes
  :type 'object)

;; 3. Define polymode object
(defcustom pm-poly/markdown
  (pm-polymode-multi-auto "markdown"
                        :hostmode 'pm-host/markdown
                        :auto-innermode 'pm-inner/markdown
                        :init-functions '(poly-markdown-remove-markdown-hooks))
  "Markdown typical configuration"
  :group 'polymodes
  :type 'object)

;; 4. Define polymode function
(define-polymode poly-markdown-mode pm-poly/markdown)
```

## Defining Backends

### Weavers
todo
### Exporters
todo
### Tanglers
todo

## Internals
### API

All API classes and methods are named with `pm-` prefix. <!-- Actual instances have -->
<!-- "/" in their name -->

Buffer local objects:

   - `pm/type`
   - `pm/chunkmode`
   - `pm/polymode`

Generics:

   - `pm-initialize` 
   - `pm-get-buffer`
   - `pm-select-buffer`
   - `pm-install-buffer`
   - `pm-get-span`
   - `pm-indent-line`
   - `pm-get-adjust-face`

Utilities:

   - `pm/get-innermost-span`
   - `pm/map-over-spans`
   - `pm/narrow-to-span`

### Initialization of polymodes

When called, `poly-XXX-mode` (created with `define-polymode`) clones
`pm-poly/XXX` object and calls `pm-initialize` generic on it. The actual
initialization depends on concrete type of the `pm-polymode` object but these
are the common steps:

   1. assign the config object into local `pm/polymode` variable
   2. clone the `pm-chunkmode` object specified by `:hostmode` slot of
   `pm-polymode`
   3. initialize hostmode by running the actual function in `:mode` slot of the
   hostmode object.
   4. store hostmode object into local `pm/chunkmode` variable
   5. set local variable `pm/type` to `'host`
   6. run `pm-polymode`'s `:init-functions` as normal hooks
   7. run `pm--setup-buffer` which is common setup function used internally to
      set `font-lock` and a range of other stuff
   8. run `poly-XXX-mode-hook`.

Discovery of the spans is done by `pm-select-buffer` generic which is commonly
called first by `jit-lock`. `pm-select-buffer` fist checks if the corresponding
`pm-chunkmode` object (and associated indirect buffer) has been already
created. If so, `pm-select-buffer` simply selects that buffer. Otherwise, it
calls `pm-install-buffer` generic which, in turn, creates `pm-chunkmode` object
and the associated indirect buffer.

