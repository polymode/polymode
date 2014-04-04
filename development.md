
# Developing with Polymode

## Class Hierarchy

Polymode uses `eieio` to represent its objects. The root class for all polymode
classes is `eieio-instance-inheritor` which provides, in addition to class
based, prototype based inheritance. This means that objects instantiated from
polymode classes can be cloned in order to dynamically create a hierarchy of
objects at runtime. As polymode uses indirect buffers to implement the
multi-mode functionality, storing mode functionality in objects (in contrast to
buffer local variables) is very convenient for moving stuff around.

Here is polymode class hierarchy:

```
  +--eieio-instance-inheritor
  |    +--polymode
  |         +--pm-submode
  |         |    +--pm-basemode
  |         |    +--pm-innermode
  |         |         +--pm-innermode-auto
  |         +--pm-config
  |              +--pm-config-multi
  |              |    +--pm-config-multi-auto
  |              +--pm-config-one
```

*Note:* Each `eieio` class has a coresponding constructor whose docstring
contains a complete description of the class. Use `C-h f pm-foo RET` to inspect
the documentation of the class. Alternatively, lookup the class definition
directly in [polymode-classes.el](polymode-classes.el).

## Polymodes and Configs

Each polymode is a function that walks and quacks like standard emacs major mode
(things like `poly-XXX-mode-map` and `poly-XXX-mode-hook` work like
expected). Polymodes can be used in place of major or minor modes, the outcome
will be identical. Plymodes are defined with `define-polymode`, which see.

Each polymode is represented by a customizable `config` object which fully
characterizes its behavior. On initialization the "representative" config object
is cloned in each new buffer.

The most important slots of root config class `pm-config` are:

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

- `pm-config-one` - Used for polymdoes with only one predefined inner mode. It
  extends `pm-config` with one slot - `innermode-name` which is a name of the
  innermode (objects of class `pm-innermode`). 
- `pm-config-multi` - Used for polymodes with multiple predefined inner
  modes. It extends `pm-config` with `innermost-names` list which contains names
  of predefined innermodes.
- `pm-config-multi-auto` - used for polymodes with multiple dymamically
  discoveroble innermodes (examples are `org-mode` and `markdown-mode`). It
  extends `pm-config-multi` with `auto-mode-name` (name of the object of class
  `pm-innermode-auto`) and `auto-innermodes` (a dynamically updated list of
  "pm-innermode" objects)


## Submodes

Submodes (basemodes and innermodes) are objects that encapsulate functionality
of the polymode submodes. The root class for all submodes, `pm-submode`,
contains among other slots:

- mode - holds the symbol of coresponding emacs mode (like `html-mode`,
`latex-mode` etc)
- buffer - holds the base emacs buffer
- indent-offset, font-lock-narrow etc - configuration options


Currently, there are three types of submode objects:

- `pm-basemode` - Represents the main mode of the buffer. Like `html-mode` in a
  web mode, `latex-mode` in noweb mode, or `org-mode` in org mode
  buffer. Currently it doesn't add any new slots to its parent class
  `pm-submode`.

- `pm-innermode` - Represents the inner modes. Various code modes in markdown,
  noweb modes or org mode are examples. `pm-innermode` extends `pm-submode` with
  additional slots, most importantly:

    * head-mode/tail-mode: emacs-modes for header/tail of the chunk
    * head-reg/tail-reg: regular expressions or functions to detect the header/tail
    * head-buffer/tail-buffer*

- `pm-innermode-auto` - Represents innermodes for which the mode type is not
  predefined and should be computed. Examples are code chunks of org and
  markdown modes. This class extends `pm-innermode` with `retriver-regexp`,
  `retriver-num` and `retriver-function` which are used to retrive the mode name
  from the header of the inner chunk.


## Defining New Modes

### One Predefined Innermode
todo:
### Multiple Predefined Innermodes
todo:
### Multiple Automatically Detected Innermodes
todo:


## Internals

todo:

For now see the header of [polymode-methods.el](polymode-methods.el).
