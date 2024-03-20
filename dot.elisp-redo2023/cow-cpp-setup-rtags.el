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
  nil)

(defun _cow-setup-rtag-proj (name)
  t)

(cow-register-project-type '(_cow-expand-rtag-proj _cow-setup-rtag-proj))

(provide 'cow-cpp-setup-rtags)
