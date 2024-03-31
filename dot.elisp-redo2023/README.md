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


# C++ IDE

Desired Capabilities 
 1. auto-complete symbols
 1. Ability to jump to file using basename instead of requiring
    user to know full path.
 1. Determine datatype of symbol under the cursor
 1. Find references to arbitrary symbol 
 1. Find references to symbol under the cursor
 1. Find declarations of arbitrary symbol
 1. Find declaration/definition of symbol under cursor
 1. cd into project directory and run compile
 1. Flip between source and header file
 1. rename symbol
 1. show inheritance tree.
 1.  find reimplementations of virthual method underneath cursor.

These are all functional when `cow-cpp-support` is set to
`'use-rtags-cpp`.  (Default out of the box setting.)  You may
customize the variable to use an LSP based IDE but COW's support of
LSP isn't fully completed.


# Internals 

## Naming Conventions
 
Public functions and variables all start with the cow- prefix.  (COW
is short for "Craig's Own Way".)

Internal functions are prefixed with _cow- if they are internal to
the defining elisp file, cowguts- if they are private to the cow-
package.

