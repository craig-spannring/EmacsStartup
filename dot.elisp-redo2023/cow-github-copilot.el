;; -*- lexical-binding: t; -*-

;;; This sets up Emacs to use github's copilot 


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



(cond
 ((boundp 'copilot--overlay-visible-p)
  (defun my/copilot-tab-or-indent ()
    "Accept copilot completion if available, otherwise indent line."
    (interactive)
    (if (copilot--overlay-visible-p)
        (copilot-accept-completion)
      (indent-for-tab-command)))
  
  (with-eval-after-load 'copilot
    (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab-or-indent)
    (define-key copilot-mode-map (kbd "TAB")   #'my/copilot-tab-or-indent)))
 (t (with-eval-after-load 'copilot
      (define-key copilot-completion-map (kbd "<tab>") #'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "TAB")   #'copilot-accept-completion))))
    

(unless (copilot-installed-version)
  (copilot-install-server))

(provide 'cow-github-copilot)
