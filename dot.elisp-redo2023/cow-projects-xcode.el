;;; -*- lexical-binding: t -*-

(require 'cow-package-helpers)
(require 'cow-projects)
(require 'cow-cpp-setup)

(use-package company        :ensure t)
(use-package flycheck       :ensure t)
(use-package popup          :ensure t)


(defun _cow-predicate-xcode-proj (proj-file)
  (string-suffix-p ".xcodeproj" proj-file))

(defun _cow-setup-xcode-proj (proj-file)
  
  (global-set-key [f9 ?f] '_cow-find-file-xcode)
  
  (list (cons 'proj-file    proj-file)
        (cons 'compile-func #'(lambda ()
                                (message "Sorry, XCode is not supported yet")))))


(defun _cow-find-file-xcode () (interactive)
        (message "Sorry, find file for xcode isn't implemented yet."))


(cowguts-register-project-type '_cow-predicate-xcode-proj
                               '_cow-setup-xcode-proj)

(provide 'cow-projects-xcode)
