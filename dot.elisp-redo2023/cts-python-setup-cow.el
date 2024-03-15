;;; Craig's Own Way (COW) of python support.

(require 'cts-package-helpers)

(message "Installing?")
(install-and-require-packages '(lsp-mode lsp-ui))

(require 'cts-lsp-common)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-auto-configure t))

(use-package lsp-mode
  :hook (python-mode . lsp-deferred) ; Replace 'java-mode' with the appropriate major mode
  :commands (lsp lsp-deferred)
  :config
  ; (lsp-enable-which-key-integration t)
  (setq lsp-auto-guess-root t) ; Automatically guess project root
  ) ; Enable which-key integration for better discovery

(provide 'cts-python-setup-cow)
