
All polymode keys start with the prefix defined by `polymode-prefix-key`
(defaults to <kbd>M-n</kbd>). The `polymode-mode-map` is the parent of all
polymodes' maps.

## Navigation

All polymode navigation commands are "cycling commands" in the sense that they
can be invoked repeatedly with the press of the basic keys. For example, if the
`polymode-minor-mode-map` is bound to the default <kbd>M-n</kbd> prefix, then
the sequence <kbd>M-n C-n C-n C-p</kbd> will invoke `polymode-next-chunk` twice
and `polymode-previous-chunk` once.

Shortcut | Description
---------|-------------
<kbd>C-n</kbd> | Move to next chunk (`polymode-next-chunk`) 
<kbd>C-p</kbd> | Move to previous chunk (`polymode-previous-chunk`) 
<kbd>C-M-n</kbd> | Move to next chunk of the same type (`polymode-next-chunk-same-type`) 
<kbd>C-M-p</kbd> | Move to previous chunk of the same type (`polymode-previous-chunk-same-type`) 

## Chunk Manipulation

Shortcut | Description
---------|-------------
<kbd>M-k</kbd> | Kill current chunk (`polymode-kill-chunk`) 
<kbd>C-t</kbd> | Toggle narrowing of the body of current chunk (`polymode-toggle-chunk-narrowing`) 
<kbd>M-m</kbd> | DWIM repeatedly mark or extend region (`polymode-mark-or-extend-chunk`, a "cycling" command)

## Exporting, Weaving and Tangling

Shortcut | Description
---------|-------------
<kbd>e</kbd> | Export the document with the current exporter. (`polymode-export`)
<kbd>E</kbd> | Ask and set the current exporter.  (`polymode-set-exporter`)
<kbd>w</kbd> | Weave the document with the current weaver. (`polymode-weave`)
<kbd>W</kbd> | Ask and set the current weaver. (`polymode-set-weaver`)
<kbd>t</kbd> | ;; not implemented yet (`polymode-tangle`)
<kbd>T</kbd> | ;; not implemented yet (`polymode-set-tangler`)
<kbd>$</kbd> | Show the process buffer is something went wrong during the processing. (`polymode-show-process-buffer`)

