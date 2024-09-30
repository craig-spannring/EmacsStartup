;;; -*- lexical-binding: t -*-

(require 'cow-package-helpers)
(require 'cow-projects)
(require 'cow-cpp-setup)

(use-package company        :ensure t)
(use-package flycheck       :ensure t)
(use-package popup          :ensure t)


(defun _cow-predicate-iarwb-proj (proj-file)
  (string-prefix-p ".ewp" proj-file))

(defun _cow-setup-iarwb-proj (proj-file)
  
  (global-set-key [f9 ?f] '_cow-find-file-iarwb)
  
  (list (cons 'proj-file    proj-file)
        (cons 'compile-func #'(lambda ()
                                (message "Sorry, IAR Workbench projects are not implemented yet")))))


(defun _cow-find-file-iarwb () (interactive)
        (message "Sorry, find file for IAR Workbench prjects aren't implemented yet."))


(cowguts-register-project-type '_cow-predicate-iarwb-proj
                               '_cow-setup-iarwb-proj)

(provide 'cow-projects-iarwb)
