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

             
(defun _cow-expand-bsr2-proj (name predicate how)
  
  (message "in (_cow-expand-bsr2-proj %s %s %s)" name predicate how)
  (cond
   ((null how)
    ;; This is a try-completion operation 
    (message "how is nil")
    nil)
   ((equal how t)
    ;; This an all-completions operation 
    (message "how is t")
    nil)
   ((equal how 'lambda)
    ;; This a test-completions operation
    (message "how is lambda")
    (funcall predicate name))
   ((and (consp how) (equal (car how) 'boundaries))
    (message "how is (boundaries . suffix)")
    nil)
   ((eq how 'metadata)
    (message "how is metadata")
    '(metadata '((category file))))
   (t
    (message "how is unhandled.  |%s|" how)
    nil)))

(defun _cow-predicate-bsr2-proj (name)
  "Determine if NAME indicates a bsr2 project"
  (and (file-exists-p name) (equal (file-name-nondirectory name) "bsr2")))

(defun _cow-setup-bsr2-proj (name)
  (message "need to setup for project %s" name)
  t)

(cow-register-project-type '_cow-expand-bsr2-proj
                           '_cow-predicate-bsr2-proj
                           '_cow-setup-bsr2-proj)

(provide 'cow-projects-bsr2)
