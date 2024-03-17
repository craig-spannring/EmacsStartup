;;; Craig's Own Way (COW) of python support.

(require 'package-helpers-cts)

(require 'lsp-common-cts)


(cond
 ((equal cts-which-lsp-package 'use-lsp)
  (use-package lsp-mode
               :ensure   t
               :init     (message "Python cow backend: lsp-mode")
               :config   (setq lsp-auto-guess-root t) ; Automatically guess project root
               :commands (lsp lsp-deferred)
               :hook (python-mode . lsp-deferred) ; Replace 'java-mode' with the appropriate major mode
               :config   (setq lsp-auto-configure t))
  
  ;; (use-package lsp-mode
  ;;              :hook (python-mode . lsp-deferred) ; Replace 'java-mode' with the appropriate major mode
  ;;              :commands (lsp lsp-deferred)
  ;;              :config
  ;;                                       ; (lsp-enable-which-key-integration t)
  ;;              (setq lsp-auto-guess-root t) ; Automatically guess project root
  ;;              ) ; Enable which-key integration for better discovery
  (install-and-require-packages '(lsp-mode lsp-ui))
  )
 ((equal cts-which-lsp-package 'use-eglot)
  (use-package eglot
               :ensure t
               :init   (message "Python cow backend: eglot")
               :config
               (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

               (setq-default eglot-workspace-configuration
                             '((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))

               :hook
               ((python-mode . eglot-ensure)))
  ))



(provide 'python-setup-cow-cts)
