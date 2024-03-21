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

             
(defun _cow-expand-rtag-proj (name predicate how)
  (message "in  (_cow-expand-rtag-proj %s %s %s)" name predicate how)
  (cond
   ((null how)
    (message "how is nil"))
   ((equal how t)
    (message "how is t"))
   ((equal how 'lambda) ;; check to see if NAME is an exact match for a project filename
    (message "how is lambda"))
   ((and (consp how) (equal (car how) 'boundaries))
        (message "how is (boundaries . suffix)"))
   ((and (consp how) (equal (car how) 'metadata))
    (message "how is metadata"))
   (t (message "how is unhandled.  |%s|" how)))
  nil)

(defun _cow-setup-rtag-proj (name)
  t)

(cow-register-project-type '(_cow-expand-rtag-proj _cow-setup-rtag-proj))

(provide 'cow-projects-bsr2)
