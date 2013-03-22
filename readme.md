

## What is polymode?

Polymode is a new emacs package that offers suport for multiple major modes
inside a single emacs buffer. It is lightweight and fully object oriented,
specifically designed for quick addition of new polymodes.

Technically speaking, polymode doesn't keep its modes in one emacs buffer but in
many indirect buffers, as many as different modes are there in a
file. Consequently polymode is as fast as switching buffers as it never
re-installs major modes after original initialization. I am very much indebted
to Dave Love's
[multi-mode.el](http://www.loveshack.ukfsn.org/emacs/multi-mode.el) for this
original idea.


## Installation 

The project is in a very early alpha stage and it is not, as yet, available in
melpa repo. You will have to install it manually:

```sh
git clone https://github.com/vitoshka/polymode.git
```

Add "polymode" directory and "polymode/modes" to your emacs path:

```lisp 
(setq load-path
      (append '("path/to/polymode/"  "path/to/polymode/modes")
              load-path))
```

Require any polymode bundles that you are interested in:

```lisp
(require 'poly-R)
(require 'poly-markdown)
```

etc


## Polymodes activation

Polymode defines emacs major and minor modes that you can use in full compliance
with emacs usual conventions.

There are two main ways to activate emacs mode. 

   1. By registering a file extension. Some of the file extensions are already
defined for you "Rmd", "Snw", "Rcpp", "cppR", "Rhtml" etc. You can find some
examples `tests` sub-directory. You can, of course, set your own extensions by
adding them to `auto-mode-alist`, which see for all the poly modes extensions
currently loaded.
   1. By setting local mode variable in you file. This is how you would activate C++R mode:
   
   ```C
   // -*- mode: poly-C++R -*-
   ```


## Naming conventions 

The core polymode object and modes are usually named as "engine+submode", or
"base_mode+submode" like "noweb+R", "markdown+R", "C++R" and "R++C". 

todo: more to come on this ... need to define object hierarchy for the full story.





