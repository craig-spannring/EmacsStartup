;;; Setup emacs.
;;;

(setq load-path (cons (expand-file-name "~/.elisp-redo2023")          load-path))
(setq load-path (cons (expand-file-name "~/.elisp-redo2023/3rdParty") load-path))

(load "pre-config-cts")

(require 'package-helpers-cts)

(setenv "PAGER" "cat")

(require 'dired-cts)
(require 'python-setup-cts)
(require 'cpp-setup-cts)
(require 'moon-configuration-cts)
(require 'vc-cts)
(require 'global-keys-cts)

(load "post-config-cts")

