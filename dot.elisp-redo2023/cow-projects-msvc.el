;;; -*- lexical-binding: t -*-

(require 'cow-package-helpers)
(require 'cow-projects)
(require 'cow-cpp-setup)

(use-package company        :ensure t)
(use-package flycheck       :ensure t)
(use-package popup          :ensure t)


(defun _cow-predicate-msvc-proj (proj-file)
  (string-prefix-p ".vcxproj" proj-file))

(defun _cow-setup-msvc-proj (proj-file)
  
  (global-set-key [f9 ?f] '_cow-find-file-msvc)
  
  (list (cons 'proj-file    proj-file)
        (cons 'compile-func #'(lambda ()
                                (message "Sorry, MSVC prjects are not implemented yet")))))


(defun _cow-find-file-msvc () (interactive)
        (message "Sorry, find file for MSVC++ project isn't implemented yet."))


(cowguts-register-project-type '_cow-predicate-msvc-proj
                               '_cow-setup-msvc-proj)

(provide 'cow-projects-msvc)
