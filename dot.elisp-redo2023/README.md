# 2023 Rewrite

Desired features
- lsp based IDE
- support for something like msvc-load-project that can handle different types of project files.
- functions from c_functions.el (probably all, but there might be one or two that get cut.)
- keys for GUD.
- probably bring in realgud-lldb
- support for different indentation styles (easily customized for working on code from different projects.)
- find out if there are more modern ways of doing syntax highlights.
- magit
- dsvn
- It should use "use-package" to simplify loading new packages. https://github.com/jwiegley/use-package


# Naming Conventions
 
Public functions and variables all start with the cow- prefix.  (COW
is short for "Craig's Own Way".)

Internal functions are prefixed with _cow- if they are internal to
the defining elisp file, cowguts- if they are private to the cow-
package.
