;;; Setup emacs.
;;;

(setq load-path (cons (expand-file-name "~/.elisp-redo2023")          load-path))
(setq load-path (cons (expand-file-name "~/.elisp-redo2023/3rdParty") load-path))

(load "cts-pre-config")

(require 'cts-package-helpers)

(setenv "PAGER" "cat")

(require 'cts-dired)
(require 'cts-python-setup)
(require 'cts-cpp-setup)
(require 'cts-moon-configuration)
(require 'cts-subversion)
(require 'cts-global-keys)

(load "cts-post-config")

