## Protection

Sometimes you don't want some functions to run in indirect buffers (inner
modes). Either you want them to run in the base (host mode) buffer or not to run
at all.

Polymode provides two generic protection functions which can be used as around advice:

- `polymode-with-current-base-buffer` - run the function inside the base buffer
- `polymode-inhibit-in-indirect-buffers` - don't run the function at all when invoked in indirect buffers

## Synchronizing State Across Polymode Buffers

Each major mode has its own buffer. This is generally a good thing and leads to
a highly composable and modularized design. Mainly because each modes does not
have to deal with other modes.

But there is a price to pay - some state and actions need to be shared across
buffers and it's not always obvious what to synchronize and what not. Things
like points, text properties, minor-modes, buffer-undo-lists, overlays are
examples of the sate that needs to be synchronized.

### Variables and Minor Modes

Polymode automatically synchronizes stated defined in the following
variables. Users and mode authors should add to these variables. In case of a
generic use cases, please open an issue with a suggestion.

- `polymode-move-these-vars-from-base-buffer` - Variables transferred from base buffer on switch to inner mode buffer.
- `polymode-move-these-vars-from-old-buffer` - Variables transferred from old buffer to new buffer on buffer switch.
- `polymode-move-these-minor-modes-from-base-buffer` - Minor modes "transferred" from the base (host-mode) buffer to inner-mode buffer on buffer switch.
- `polymode-move-these-minor-modes-from-old-buffer` -  Minor modes "transferred" from the old buffer to new buffer on buffer switch.

Polymode also runs the following hooks on each buffer switch. Use these hooks to
synchronize your specific state if it cannot be done with the above generic
variables.

- `polymode-before-switch-buffer-hook`
- `polymode-after-switch-buffer-hook`


### Hooks

By default Emacs runs `pre-command-hook` and `post-command-hook` only in the
original buffer. On a few occasions actions should propagate to other
buffers. Most commonly such actions manipulate and update local state in the
corresponding buffers.

Polymode maintains a list of functions which, if present in the hook, should be
run in other buffers.

- `polymode-run-these-pre-commands-in-other-buffers`  - Commands to run in all other buffers regardless where command originated.
- `polymode-run-these-post-commands-in-other-buffers` - idem but for post-command-hook

- `polymode-run-these-before-change-functions-in-other-buffers`  - Functions to run in all other buffers regardless where command originated.
- `polymode-run-these-after-change-functions-in-other-buffers` - idem but for after-change-fun
