;;; Use rtags for C++ support

(require 'cow-package-helpers)

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

             
             

(provide 'cow-cpp-setup-rtags)
