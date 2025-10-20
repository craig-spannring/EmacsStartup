;;; This sets up Emacs to use github's copilot 
;;;

(message "Setting up copilot")

(use-package s            :ensure t)
(use-package dash         :ensure t)
(use-package editorconfig :ensure t)
(use-package company      :ensure t)
(use-package jsonrpc      :ensure t)

(require 'cow-cpp-setup)

(defun cow-copilot-mode-hook-func()
  (message "running cow-copilot-mode-hook-func")
  (copilot-mode))

(use-package copilot
  :ensure    t
  :defer     nil
  :hook      (prog-mode . cow-copilot-mode-hook-func)
  :custom    (copilot-idle-delay 1))       ; <--- less distractions


(keymap-set c++-mode-map "C-S-<tab>" #'copilot-accept-completion)
(keymap-set c++-mode-map "C-s-<tab>" #'copilot-accept-completion-by-word)
(keymap-set c-mode-map "C-S-<tab>"   #'copilot-accept-completion)
(keymap-set c-mode-map "C-s-<tab>"   #'copilot-accept-completion-by-word)

(unless (copilot-installed-version)
  (copilot-install-server))

(provide 'cow-github-copilot)
