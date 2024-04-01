;;; Craig's Own Way (COW) of python support.

(require 'cow-package-helpers)

(require 'cow-lsp-common)


(cond
 ((equal cow-which-lsp-package 'use-lsp)
  (use-package lsp-mode
               :ensure   t
               :init     (message "Python cow backend: lsp-mode")
               ;; :config   (setq lsp-auto-guess-root t) ; Automatically guess project root
               :commands (lsp lsp-deferred)
               :hook (python-mode . lsp-deferred) ; 
               :config   (setq lsp-auto-configure t))
  
  (install-and-require-packages '(lsp-mode lsp-ui)) ;; todo- replace with use-package
  (mapc #'(lambda (exe)
	    (when (not (executable-find exe))
	      (message "Warning: Could not find %s.\nRequired for full python support." exe)
	      (sleep-for 1.5)))
	'("pylsp" "flake8")))
 ((equal cow-which-lsp-package 'use-eglot)
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



(provide 'cow-python-setup-cow)
