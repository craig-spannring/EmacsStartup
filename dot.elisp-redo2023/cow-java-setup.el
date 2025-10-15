;;; This sets up a Java IDE
;;;
;;; See README.md for more details.


(message "Loading Java support.")

(require 'cow-package-helpers)
(require 'cow-complete-common)
(require 'cow-lsp-common)

(install-and-require-packages '(lsp-java company))

(add-hook 'java-mode-hook #'lsp)


(cond
 ((equal cow-which-lsp-package 'use-eglot)
  (message "COW support of Java with eglot is limited."))
 ((equal cow-which-lsp-package 'use-lsp)
  (use-package projectile  :ensure t)
  (use-package flycheck    :ensure t)
  (use-package yasnippet   :ensure t :config (yas-global-mode))
  (use-package lsp-mode    :ensure t :hook ((lsp-mode . lsp-enable-which-key-integration)))
  (use-package hydra       :ensure t)
  (use-package company     :ensure t)
  (use-package lsp-ui      :ensure t)
  (use-package which-key   :ensure t :config (which-key-mode))
  (use-package lsp-java    :ensure t :config (add-hook 'java-mode-hook 'lsp))
  (use-package dap-mode    :ensure t :after lsp-mode :config (dap-auto-configure-mode))
  (use-package dap-java    :ensure nil)
  (use-package helm-lsp    :ensure t)
  (use-package helm        :config (helm-mode))
  (use-package lsp-treemacs)))
  
(provide 'cow-java-setup)

