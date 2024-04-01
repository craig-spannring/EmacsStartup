;;; Support for a BSR2 project. 

(require 'cow-package-helpers)
(require 'cow-projects)
(require 'cow-cpp-setup)

(use-package company        :ensure t)
(use-package flycheck       :ensure t)
(use-package popup          :ensure t)


(defun _cow-brs2-to-compile-commands-json (proj-file)
  (concat (file-name-directory proj-file) "SystemsSrc/compile_commands.json"))

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
  
  (cond
   ((equal cow-cpp-support 'use-rtags-cpp)
    ;; Fail if we can't find rdm
    (if (not (rtags-executable-find "rdm")) 
	(error "Error: couldn't find rdm"))
    (cow-rdm-select-a-compile-commands-json
     (_cow-brs2-to-compile-commands-json proj-file)))
   ((equal cow-cpp-support 'use-lsp-cpp)
    (cow-clangd-select-a-compile-commands-json
     (_cow-brs2-to-compile-commands-json proj-file))))

  
  ;; 
  ;; cowguts-register-project-type wants the setup function to return
  ;; a plist with the keys proj-file and compile-func.  So create and
  ;; return such a plist.
  ;; 
  (list (cons 'proj-file    proj-file)
	(cons 'compile-func #'(lambda ()
				;; let user modify compile command 
				(let ((cmd (read-from-minibuffer
					    "Compile project command: "
					    compile-command nil nil
					    '(compile-history . 1))))
				  ;; Once user is happy, run the compile command. 
				  (compile cmd))))))

(cowguts-register-project-type '_cow-predicate-bsr2-proj
                               '_cow-setup-bsr2-proj)

(provide 'cow-projects-bsr2)
