Internally Polymode uses indirect buffers to implement inner spans of different
modes. Each inner major mode within a polymode buffer has its own indirect
buffer associated with it. Host mode lives in the base buffer. Therefore,
switching modes in Polymode is as fast as switching emacs buffers because it
never re-installs major modes like most of other MMM frameworks do. Dave Love's
[multi-mode.el](http://www.loveshack.ukfsn.org/emacs/multi-mode.el) gets full
credit for this idea.

## Terminology

  - _**span**_ is a contiguous and functionally homogeneous region of text of the
   same Emacs major mode. On functional grounds, there are 4 types of spans:
   head, body, tail and host. In the above example there are 5 spans - org host,
   elisp org header, elisp body, elisp org tail and org host.<br> All spans are
   closed on the left and open on the right. For example, a span spanning [3, 5)
   contains character at position 3 but not at position 5. This is consisten
   with how emacs' functions dealing with text properties work.

 - _**chunk**_ is a contiguous region that consists of one or more _spans_. There
   are two major types of chunks:
    - _**host chunks**_ which contain only one span - host mode, and 
    - _**inner chunks**_ which commonly consists of head, body and tail, where body
      is of some other mode than host.<br>
   
 - _**polymode**_, just like an emacs standard mode, means one of the following:
 
    1. an _generic term_ for a collection of related functionality that is
      available in emacs buffers
    2. _function_ which installs a bunch of functionality into emacs buffers.
      Therefore, you can use polymodes just as any other emacs mode.<br>


## Configuration

Host and inner chunks are configured through _objects_ derived from
`pm-chunkmode` class and are generically referred to as "chunkmodes". These
objects have named of the form `pm-host/NAME` and `pm-inner/NAME` and are
grouped into `poly-hostmodes` and `poly-innermodes` customization groups
respectively. During the initialization chunkmodes are cloned and stored in
buffer local variable `pm/chunkmode.`

Polymodes are configured through _objects_ of class `pm-polymode` which are
named with `pm-poly/NAME` scheme where `NAME` is the root name of the polymode
`poly-NAME-mode`. During initialization of the polymodes the `pm-poly/NAME`
object is cloned and stored in a buffer-local variable `pm/polymode` which is
common across all indirect buffers.

It is worth pointing out the difference between `chunks` and `chunkmodes`.
Chunks are fragments of text and there might be multiple chunks of the same mode
within a buffer. In contrast, there is only one chunkmode of some type per
buffer and all its chunks "share" the chunkmode. At any point of time, each
chunkmode is attached to its own buffer (base or indirect) and one emacs major
mode.

 
## Class Hierarchy

Polymode package uses `eieio` to represent its objects. The root class for all
polymode classes is `eieio-instance-inheritor` which provides prototype based
inheritance. This means that objects instantiated from polymode classes can be
cloned in order to dynamically create a hierarchy of customizable objects. There
are a bunch of such objects already defined, you can check those in `polymodes`,
`hostmodes` and `innermodes` customization groups.

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

## Naming Conventions

User facing functionality is named with `polymode-` prefix. Polymodes are named
with `poly-NAME-mode` convention. Host, inner and polymode configuration objects
start with `pm-host/`, `pm-inner/` and `pm-poly/` prefixes
respectively. Classes, methods and development API have `pm-` prefix.
