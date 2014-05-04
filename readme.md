## Overview

Polymode is an emacs package that offers support for multiple major modes inside
a single emacs buffer. It is lightweight and fully object oriented, specifically
designed for quick addition of new polymodes.

Technically speaking, polymode doesn't keep its modes in a single emacs buffer
but in several indirect buffers, actually as many as different modes are there
in a file. Consequently, polymode is as fast as switching buffers because it
never re-installs major modes. I am very much indebted to Dave Love's
[multi-mode.el](http://www.loveshack.ukfsn.org/emacs/multi-mode.el) for this
awesome idea.


## Installation 

The project is in an alpha stage and it is not, as yet, available in melpa
repo. You will have to install it manually:

```sh
git clone https://github.com/vitoshka/polymode.git
```

Add "polymode" directory and "polymode/modes" to your emacs path:

```lisp 
(setq load-path
      (append '("path/to/polymode/"  "path/to/polymode/modes")
              load-path))
```

Require any polymode bundles that you are interested in. For example:

```lisp
(require 'poly-R)
(require 'poly-markdown)
```

Note that for the full use of poly-markdown modes you will need
[markdown-mode.el](http://jblevins.org/projects/markdown-mode/). It is also
available from MELPA repository.


## Polymodes activation

Polymode defines emacs major and minor modes that you can use in full compliance
with emacs usual conventions.

There are two main ways to activate emacs mode. 

   1. _By registering a file extension_ by adding the to `auto-mode-alist`. Have
      a look at [polymode-configuration.el](polymode-configuration.el) and pick
      the once you are using. Example are:

    ```lisp
    ;;; MARKDOWN
    (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

    ;;; R related modes
    (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
    ```

   1. _By setting local mode variable in you file_:
   
   ```c++
   // -*- mode: poly-C++R -*-
   ```
   or 
   ```sh
   ## -*- mode: poly-brew+R; -*-
   ```

## Warning

  * Tested with Emacs 24.3.1
  * Does not work in emacs devel. They removed jit-lock from indirect buffers
    recenly. I am looking for workarounds.


Some things still don't work as expected. For example:
    
   * To kill a polymode buffer you will have position the cursor in the base mode buffer. 
   * Customization interface is not working as expected (an eieio bug) and is
     not tested. 

## Developing with Polymode

[development.md](development.md)

## Screenshots

### Ess-help buffer

<img src="img/ess-help.png" width="350px"/>

### C++R
<img src="img/cppR.png" width="350px"/>

### markdown+R

<img src="img/Rmd.png" width="350px"/>

### markdown+R+YAML

<img src="img/rapport.png" width="350px"/>

