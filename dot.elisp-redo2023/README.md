# 2023 Rewrite

Desired features
- lsp based IDE
- support for something like msvc-load-project that can handle different types of project files.
- functions from c_functions.el (probably all, but there might be one or two that get cut.)
- key bindings for GUD.
- probably bring in realgud-lldb
- support for different indentation styles (easily customized for working on code from different projects.)
- find out if there are more modern ways of doing syntax highlights.
- magit
- dsvn
- It should take advantage of "use-package" to simplify loading new packages. https://github.com/jwiegley/use-package

# Prerequisites
TBD

# C++ IDE
## Prerequsites
- clangd and/or rtags



This supports two different options for C++ support.  The option is
controlled by the customization variable `cow-cpp-support`.

You may load a project with `M-x cow-load-project`.

# Python IDE 
## Prerequsites
- pylsp

TBD 

# Calendar Display 

We provide a latitude/longitude for the Emacs calendar (`M-x
calendar`) via the `cow-calendar-location` variable.  You may
customize that variable.

# Internals 

## Naming Conventions
 
Public functions and variables all start with the cow- prefix.  (COW
is short for "Craig's Own Way".)

Internal functions are prefixed with _cow- if they are internal to
the defining elisp file, cowguts- if they are private to the cow-
package.

