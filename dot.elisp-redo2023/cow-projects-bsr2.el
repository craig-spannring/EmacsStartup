;;; Use rtags for C++ support

(require 'cow-package-helpers)
(require 'cow-projects)


(use-package company        :ensure t)
(use-package company-rtags  :ensure t)
(use-package flycheck       :ensure t)
(use-package flycheck-rtags :ensure t)
(use-package popup          :ensure t)

(use-package rtags
             :ensure t
             :config (setq rtags-completions-enabled t)
             :config (setq rtags-use_helm            t)
             :config (setq rtags-display-result-backend      'helm))

(defun _cow-predicate-bsr2-proj (name)
  "Determine if NAME indicates a bsr2 project"
	  (and (file-exists-p name)
	       (equal (file-name-nondirectory name) "bsr2")))

(defun _cow-setup-bsr2-proj (proj-file)
  (message "need to setup for project %s" proj-file)
  (setq compile-command
        (concat 
         (abbreviate-file-name
          (expand-file-name (concat
                             (file-name-directory proj-file)
                             "./bsr2")))
         " build"))
  (list (cons 'proj-file    proj-file)
	(cons 'compile-func #'(lambda ()
				(let ((cmd (read-from-minibuffer
					    "Compile project command: "
					    compile-command nil nil
					    '(compile-history . 1))))
				  (compile cmd))))))

(cow-register-project-type '_cow-predicate-bsr2-proj
                           '_cow-setup-bsr2-proj)

(provide 'cow-projects-bsr2)
