;;; Setup emacs.
;;;

(setq load-path (cons (file-name-directory load-file-name)
                      load-path))

(load "cow-pre-config")

(require 'cow-package-helpers)
(require 'cow-dired)
(require 'cow-python-setup)
(require 'cow-cpp-setup)
(require 'cow-moon-configuration)
(require 'cow-vc)
(require 'cow-global-keys)
(require 'cow-projects)
(require 'cow-projects-bsr2)

(load "cow-post-config")

