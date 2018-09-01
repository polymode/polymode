
*!! Polymode required Emacs version 25.1 or higher!!*

## Via package.el

Polymode is available on the two major `package.el` community maintained repos -
[MELPA Stable](http://stable.melpa.org) and [MELPA](http://melpa.org).

You can install Polymode, or more commonly a specific polymode, interactively

<kbd>M-x package-install [RET] polymode [RET] </kbd>
<kbd>M-x package-install [RET] poly-markdown [RET] </kbd><br>
...

In order to see all available polymodes do <kbd>M-x list-packages [RET]</kbd>
and search for `poly-` prefix.

To automatically install relevant polymodes put the following lines into
`.emacs` for each polymode that you use:

```el
(unless (package-installed-p 'polymode)
  (package-install 'poly-markdown))
```

You can pin packages to always install from MELPA stable like follows: 

```el
(add-to-list 'package-pinned-packages '(poly-markdown . "melpa-stable") t)
```

## Via use-package

`use-package` can also be used to install Polymode via the `package.el`.

```el
(use-package poly-markdown
  :ensure t)
```

Or from MELPA stable:

```el
(use-package poly-markdown
  :ensure t
  :pin melpa-stable)
```

For further configuration options with `use-package` see the official
[use-package repository](https://github.com/jwiegley/use-package) or <kbd>C-h v
use-package [RET]</kbd>.

## Manually


```sh
cd path/to/vc/dir
git clone https://github.com/polymode/polymode
git clone https://github.com/polymode/poly-markdown
...
```

In `.emacs` add "polymode" directory and "polymode/modes" to the load path:

```el
(setq load-path
      (append '("path/to/vc/dir/polymode/"  "path/to/vc/dir/poly-markdown/")
              load-path))
```

Require any polymode bundles that you are interested in. For example:

```el
(require 'poly-markdown)
```

## Activation of Polymodes

Polymodes are functions and can be used just like ordinary emacs modes.
Particularly, activate them in files by either registering a file extension in
`auto-mode-alist`
```el
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
```

or by setting local major mode variable in the header of the file:

```nw
   // -*- mode: poly-noweb -*-
```

Normally you won't need to setup modes yourself as polymode packages register
their polymodes with the common extensions automatically. See the documentation
of each polymode package.
