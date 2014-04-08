(defgroup polymode-weave nil
  "Polymode Weavers"
  :group 'polymode)

(defclass pm-weaver (polymode)
  ((from-to
    :initarg :from-to
    :initform '()
    :type list
    :custom list
    :documentation
    "A list of lists of the form (from-ext to-ext doc string-or-function).")
   (function
    :initarg :function
    :init-value nil
    :type (or null function)
    :documentation
    "Actuall function to be called with ..."
    )
   (sentinel
    :initarg :function
    :init-value nil
    :type (or null function)
    :documentation
    "Sentinel to be called ..."
    ))
  "Exporter class")

(defmacro polymode-register-weaver (name &rest polymodes)
  "Register weaver NAME with all POLYMODES.

By convention weaver names start with 'pm-weaver/'.

Weavers are alists that of the form

 ((extension1 . function-or-string1)
  (extension2 . function-or-string2)
  ...
  )

Polymode names are either full names of the config
objects (e.g. pm-config/markdown) or a short name of the
mode (e.g. markdown).

'extensionI' is either a file name extension or a list of
extensions that this weaver can process.

'function-or-stringI' can be either a function of one argument,
input file name, or a command to be executed on the current file
if it has the extension '.extensionI'.

Last 'extensionN' can be t, in which case function-or-stringN is
called on the current file irrespective of its extension.


Weavers are inherited by config objects. So for example if
pm-config/B inherits (is cloned) from pm-config/A and you
registered weaver pm-weaver
"
  ...)


