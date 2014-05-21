_This doc is an early draft._

# Developing with Polymode

Polymode doesn't keep its modes in a single emacs buffer but in several indirect
buffers, actually as many as different modes are there in a file. Consequently,
polymode is as fast as switching buffers because it never re-installs major
modes. I am very much indebted to Dave Love's
[multi-mode.el](http://www.loveshack.ukfsn.org/emacs/multi-mode.el) for this
awesome idea.

- [Glossary of Terms](#glossary-of-terms)
- [Class Hierarchy](#class-hierarchy)
- [Polymodes and Configs](#polymodes-and-configs)
- [Submodes](#submodes)
- [Defining New Modes](#defining-new-modes)
  - [One Predefined Chunkmode](#one-predefined-chunkmode)
  - [Multiple Predefined Chunkmodes](#multiple-predefined-chunkmodes)
  - [Multiple Automatically Detected Chunkmodes](#multiple-automatically-detected-chunmodes)
- [Defining Literate Programming Backends](#defining-backends-weavers-exporters-and-tanglers)
  - [Weavers](#weavers)
  - [Exporters](#exporters)
  - [Tanglers](#tanglers)
- [Internals](#internals)
  - [Initialization of polymodes](#initialization-of-polymodes)
  - [API](#api)

## Glossary of Terms

_naming conventions are subject to change_

 - span:
 - chunk:
 - polymode:
 - submode:
 - basemode:
 - chunkmode:

## Class Hierarchy

Polymode uses `eieio` to represent its objects. The root class for all polymode
classes is `eieio-instance-inheritor` which provides, in addition to class
based, prototype based inheritance. This means that objects instantiated from
polymode classes can be cloned in order to dynamically create a hierarchy of
objects at runtime. As polymode uses indirect buffers to implement the
multi-mode functionality, storing mode functionality in objects (in contrast to
buffer local variables) is very convenient for moving stuff around.

Current polymode class hierarchy:

```
  +--eieio-instance-inheritor
  |    +--polymode-root
  |         |
  |         +--pm-polymode
  |         |    +--pm-polymode-multi
  |         |    |    +--pm-polymode-multi-auto
  |         |    +--pm-polymode-one
  |         |
  |         +--pm-submode
  |         |    +--pm-chunkmode
  |         |    |    +--pm-chunkmode-auto
  |         |    +--pm-basemode
  |         |
  |         |
  |         +--pm-weaver
  |         |    +--pm-shell-weaver
  |         |    +--pm-callback-weaver
  |         +--pm-exporter
  |              +--pm-shell-exporter
  |              +--pm-callback-exporter

```

*Note:* Each `eieio` class has a corresponding constructor whose docstring
contains a complete description of the class. Use `C-h f pm-foo RET` to inspect
the documentation of the class (available in recent emacses
only). Alternatively, lookup the class definition directly in
[polymode-classes.el](polymode-classes.el).

## Polymodes and Configs

Each polymode is a function that walks and quacks like standard emacs major mode
(things like `poly-XXX-mode-map` and `poly-XXX-mode-hook` work like
expected). Polymodes can be used in place of major or minor modes, the outcome
will be identical. Plymodes are defined with `define-polymode`, which see.

Each polymode is represented by a customizable `config` object which fully
characterizes its behavior. On polymode initialization this config object is
cloned in every new buffer.

The most important slots of root config class `pm-polymode` are:

- `basemode-name` - name of the object of class `pm-basemode`, see below.
- `minor-mode-name` - name of the minor mode which is activated in all indirect
  buffers. This is the glue that connects all buffers. By default it is
  `polymode-minor-mode`. It is unlikely that developers will need to change
  this.
- `basemode` - dynamically set to the basemode object generated at
  initialization time.
- `buffers` - holds all indirect buffers associated with the base buffer for
  easy access.

Currently there are three types of config objects:

- `pm-polymode-one` - Used for polymdoes with only one predefined chunkmode. It
  extends `pm-polymode` with one slot - `chunkmode-name` which is a name of the
  chunkmode (objects of class `pm-chunkmode`).
- `pm-polymode-multi` - Used for polymodes with multiple predefined inner
  modes. It extends `pm-polymode` with `chunkmodes` list which contains names of
  predefined `pm-chunkmode` objects.
- `pm-polymode-multi-auto` - used for polymodes with multiple dymamically
  discoveroble chunkmodes (examples are `org-mode` and `markdown-mode`). It
  extends `pm-polymode-multi` with `auto-mode-name` (name of the object of class
  `pm-chunkmode-auto`) and `auto-chunkmodes` (a dynamically updated list of
  "pm-chunkmode" objects)


## Submodes

Submodes (basemodes and chunkmodes) are objects that encapsulate functionality
of the polymode's submodes. The root class for all submodes, `pm-submode`,
contains among other slots:

- mode - holds the symbol of corresponding emacs mode (like `html-mode`,
`latex-mode` etc)
- buffer - holds the base emacs buffer
- indent-offset, font-lock-narrow etc - configuration options


Currently, there are three types of submode objects:

- `pm-basemode` - Represents the main mode of the buffer. Like `html-mode` in a
  web mode, `latex-mode` in noweb mode, or `org-mode` in org mode
  buffer. Currently it doesn't add any new slots to its parent class
  `pm-submode`.

- `pm-chunkmode` - Represents the inner modes. Various code modes in markdown,
  noweb modes or org mode are examples. `pm-chunkmode` extends `pm-submode` with
  additional slots, most importantly:

    * head-mode/tail-mode: emacs-modes for header/tail of the chunk
    * head-reg/tail-reg: regular expressions or functions to detect the header/tail
    * head-buffer/tail-buffer*

- `pm-chunkmode-auto` - Represents chunkmodes for which the mode type is not
  predefined and should be computed. Examples are code chunks of org and
  markdown modes. This class extends `pm-chunkmode` with `retriver-regexp`,
  `retriver-num` and `retriver-function` which are used to retrive the mode name
  from the header of the inner chunk.


## Defining New Modes

### One Predefined Chunkmode

(noweb)

### Multiple Predefined Chunkmodes

(web-mode)

### Multiple Automatically Detected Chunkmodes

(markdown,  org-mode etc)

## Defining Backends

### Weavers
todo
### Exporters
todo
### Tanglers
todo

## Internals

### Initialization of polymodes

Initialization depends on concrete type of the `pm/polymode` object, but the main
steps are described here.

`poly-XXX-mode` is created with `(define-polymode poly-XXX-mode pm-poly/XXX)`
where `pm-poly/XXX` is a predefined `pm-polymode` object representing the mode.

When called, `poly-XXX-mode` clones `pm-poly/XXX` object and calls
`pm/initialize` on it.  In turn, `pm/initialize` performs the following steps:

   1. assign the config object into local `pm/polymode` variable
   2. clone the `pm/submode` objects specified by `:basemode` slot of
   `pm/polymode`
   3. initialize base-mode by running the actual function in `:mode` slot of
   the basemode object. 
   4. store basemode object into local `pm/submode` variable 
   5. set local variable `pm/type` to `'base` 
   6. run `pm/polymode`'s `:init-functions` as normal hooks
   7. run `pm--setup-buffer` which is common setup function used internally to
      set font-lock and a range of workarounds
   8. run `poly-XXX-mode-hook`.

Discovery of the chunk spans is done by `pm/select-buffer` generic which is
commonly called first by jit-lock. `pm/select-buffer` fist checks if the
corresponding `pm-chunkmode` object and associated indirect buffer has been
already created. If so, `pm/select-buffer` simply selects the buffer. Otherwise,
it calls `pm/install-buffer` in order to create `pm-chunkmode` object and
initialize the indirect buffer.

### API

All API objects, functions and methods are named with `pm/` prefix.

Objects

   - `pm/type`
   - `pm/submode`
   - `pm/polymode`

Generics:

   - `pm/initialize` 
   - `pm/get-buffer`
   - `pm/select-buffer`
   - `pm/install-buffer`
   - `pm/get-span`
   - `pm/indent-line`
   - `pm/get-adjust-face`

Utilities:

   - `pm/get-innermost-span`
   - `pm/map-over-spans`
   - `pm/narrow-to-span`

