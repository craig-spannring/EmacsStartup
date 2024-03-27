;;; Use rtags for C++ support

(require 'cow-package-helpers)
(require 'cow-projects)

(use-package company        :ensure t)
(use-package flycheck       :ensure t)
(use-package popup          :ensure t)


(defun _cow-predicate-bsr2-proj (name)
  "Determine if NAME indicates a bsr2 project"
	  (and (file-exists-p name)
	       (equal (file-name-nondirectory name) "bsr2")))

(defun _cow-setup-bsr2-proj (proj-file)
  "File that can be called by cow-load-project.

In addition to the requirements imposed by cow-load-project
this will optionally start rdm and connect with that server 
to enable code browsing. 
"
  (message "need to setup for project %s" proj-file)
  (setq compile-command
        (concat 
         (abbreviate-file-name
          (expand-file-name (concat
                             (file-name-directory proj-file)
                             "./bsr2")))
         " build"))

  ;; 
  ;; cowguts-register-project-type wants the setup function to return
  ;; a plist with the keys proj-file and compile-func.  So create and
  ;; return a plist.
  ;; 
  (list (cons 'proj-file    proj-file)
	(cons 'compile-func #'(lambda ()
				;; let use modify compile command 
				(let ((cmd (read-from-minibuffer
					    "Compile project command: "
					    compile-command nil nil
					    '(compile-history . 1))))
				  ;; Once user is happy, run the compile command. 
				  (compile cmd))))))

(cowguts-register-project-type '_cow-predicate-bsr2-proj
                               '_cow-setup-bsr2-proj)

(provide 'cow-projects-bsr2)
