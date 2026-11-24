;;; -*- lexical-binding: t; -*-

;;; Setup emacs.  


(when (and (boundp 'load-file-name) load-file-name)
  (message "load-file-name %s %s" load-file-name (boundp 'load-file-name))
  (setq load-path (cons (file-name-directory load-file-name)
			load-path))
  (setq load-path (cons (file-name-concat (file-name-directory load-file-name) "3rdParty")
			load-path)))

(require 'cow-host-specific-pre)

(load "cow-pre-config")

(require 'cow-helpers)
(require 'cow-package-helpers)
(require 'cow-dired)
(require 'cow-python-setup)
(require 'cow-cpp-setup)
(require 'cow-java-setup)
(require 'cow-debugger-setup)
(require 'cow-moon-configuration)
(require 'cow-vc)
(require 'cow-projects)
(require 'cow-projects-cmake)
(require 'cow-projects-bsr2)
(require 'cow-projects-xcode)
(require 'cow-projects-msvc)
(require 'cow-projects-iarwb)
(require 'cow-github-copilot)
(require 'cow-qt-support)
(require 'cow-sql)
(require 'cow-uml)
(require 'cow-keys-global)
(require 'cow-host-specific-post)
      
(load "cow-post-config")

