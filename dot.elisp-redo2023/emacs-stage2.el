;;; Setup emacs.
;;;

(setq load-path (cons (expand-file-name "~/.elisp-redo2023") load-path))

(require 'cts-package-helpers)

(setenv "PAGER" "cat")
(setq load-path (cons (expand-file-name "~/.elisp-redo2018") load-path))
 
(require 'dired-x)

(load "cts-python-setup")
