# EmacsStartup

A .emacs file and other startup/initialization files. 

To install 
```shell
    $ git clone https://github.com/craig-spannring/EmacsStartup.git
    $ cd EmacsStartup 
    $ ./install-symlinks 
```
Note- the first time you run Emacs it will spend a fair amount of time
downloading and installing pakcages.


## History 

The startup files have accumulated over the last 30+ years.  It has
accumulated some cruft over the years.  In 2018 I sat down and started
refactoring and removed support for ancient capabilities (e.g. Stratus
VOS) from the elisp, but it was still crufty.  In 2023 I started a
second round, This most recent refactor wasn't so much a refactor as
it was a delete everything and rewrite anything I still wanted.

The 2023 version is the currently supported version and is selected by
default.  If you wish to use the 2018 version simply set the
environment variable `COW_SETUP_ERA` to `2018`.

For more details on the 2023 version, see dot.elisp-redo2023/README.md
