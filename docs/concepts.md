Internally Polymode uses indirect buffers to implement inner spans of different
modes. Each inner major mode within a polymode buffer has its own indirect
buffer associated with it. Host mode lives in the base buffer. Therefore,
switching modes in Polymode is as fast as switching emacs buffers because it
never re-installs major modes like most of other MMM frameworks do. Dave Love's
[multi-mode.el](http://www.loveshack.ukfsn.org/emacs/multi-mode.el) gets full
credit for this idea.

By _sibling buffers_ we mean base or indirect buffers which are shared with the
current buffer.

## Terminology

  - _**span**_ is a contiguous and homogeneous region of text of the same Emacs
   major mode. There are 4 types of spans: head, body, tail and host. <br> All
   spans are closed on the left and open on the right. For example, a span
   spanning [3, 5) contains character at position 3 but not at position 5. This
   is consistent with how emacs' functions deal with text properties.

 - _**chunk**_ is a contiguous region that consists of one or more _spans_. There
   are two major types of chunks:

    - _**host chunks**_ which contain only one span in the host major mode.
    - _**inner chunks**_ which commonly consists of head, body and tail, where
      body is of some other mode than host.<br>

 - _**polymode**_ means one of the following:

    1. a _sum_ of related functionality available in emacs buffers
    2. _function_ which installs a bunch of functionality into emacs
      buffers. <br> Polymodes can be used like any other emacs major mode (for
      example placed in `auto-mode-alist`), or like a minor mode in order to
      install functionality on top of an existing emacs mode.<br>
    3. an _object_ of class `pm-polymode` which holds the configuration for the
       polymode.

 - _**chunkmode**_, and the more specific _**hostmode**_ and _**innermode**_ mean either:

    1. a _sum_  of related functionality in host and inner chunks
    2. configuration _objects_ derived from `pm-chunkmode` class
       (`pm-host-hostmode`, `pm-inner-innermode`, `pm-inner-auto-chunkmode`).


## Configuration

Host and inner chunks are configured through _objects_ derived from
`pm-chunkmode` class and are generically referred to as "chunkmodes". These
objects have named of the form `poly-XYZ-hostmode` and `poly-XYZ-innermode`
and are refereed to as `hostmodes` and `innermodes` respectively.

Polymodes are configured through _objects_ of class `pm-polymode` which are
named with `poly-XYZ-polymode` scheme where `XYZ` is the same as in the
polymode function `poly-XYZ-mode`. During initialization of the polymodes the
`poly-XYZ-polymode` object is cloned and stored in a buffer-local variable
`pm/polymode`. This object is shared across all sibling buffers.

`Chunks` and `chunkmodes` are different concepts. Chunk is a fragments of text
and there might be multiple chunks of the same mode within a buffer. In
contrast, there is only one chunkmode of some type per buffer and the "behavior"
in each chunk is determined by the chunkmode.


## Class Hierarchy

Polymode package uses `eieio` to represent its objects. The root class for all
polymode classes is `eieio-instance-inheritor` which provides prototype based
inheritance. This means that objects instantiated from polymode classes can be
cloned in order to dynamically create a hierarchy of customizable objects.

Instance inheritance (or prototype inheritance) means that if you change a slot
of a parent object, all of the children and grand-children of the object will
inherit the new value unless they explicitly overwrite that slot.

There are a bunch of such objects already defined, you can check those in
`polymodes`, `poly-hostmodes` and `poly-innermodes` customization groups.

Polymode class hierarchy:

```
  +--eieio-instance-inheritor
       +--pm-root
            +--pm-polymode
            +--pm-chunkmode
            |    +--pm-inner-chunkmode
            |    |    +--pm-inner-auto-chunkmode
            |    +--pm-host-chunkmode
            +--pm-weaver
            |    +--pm-shell-weaver
            |    +--pm-callback-weaver
            +--pm-exporter
                 +--pm-shell-exporter
                 +--pm-callback-exporter
```

*Using Help with EIEIO:* Each `eieio` class has a corresponding constructor
whose docstring contains a complete description of the class.

## General Naming Conventions

User facing functionality is named with `polymode-` prefix. Polymodes are named
with `poly-XYZ-mode` convention. Host, inner and polymode configuration objects
are named `poly-XYZ-hostmode`, `poly-XYZ-innermode` and
`poly-name-polymode`. Classes, methods and developer-oriented API have `pm-`
prefix.
