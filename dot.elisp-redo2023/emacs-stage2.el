;;; Setup emacs.
;;;

(setq load-path (cons (file-name-directory load-file-name)
                      load-path))

(load "pre-config-cts")

(require 'package-helpers-cts)

(setenv "PAGER" "cat")

(require 'dired-cts)
(require 'python-setup-cts)
(require 'cpp-setup-cts)
(require 'moon-configuration-cts)
(require 'vc-cts)
(require 'global-keys-cts)
(require 'projects-cts)

(load "post-config-cts")

