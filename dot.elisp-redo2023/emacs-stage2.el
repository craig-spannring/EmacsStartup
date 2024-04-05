;;; Setup emacs.
;;;

(setq load-path (cons (file-name-directory load-file-name)
                      load-path))

(load "cow-pre-config")

(require 'cow-helpers)
(require 'cow-package-helpers)
(require 'cow-dired)
(require 'cow-python-setup)
(require 'cow-cpp-setup)
(require 'cow-gud-setup)
(require 'cow-moon-configuration)
(require 'cow-vc)
(require 'cow-global-keys)
(require 'cow-projects)
(require 'cow-projects-cmake)
(require 'cow-projects-bsr2)
(require 'cow-projects-xcode)
(require 'cow-projects-msvc)
(require 'cow-projects-iarwb)
(require 'cow-qt-support)
(require 'cow-sql)

(load "cow-post-config")

