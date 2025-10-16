;;; This sets up Emacs to use github's copilot 
;;;

(message "Setting up copilot")

(use-package s            :ensure t)
(use-package dash         :ensure t)
(use-package editorconfig :ensure t)
(use-package company      :ensure t)
(use-package jsonrpc      :ensure t)


(defun cow-copilot-mode-hook-func()
  (message "running cow-copilot-mode-hook-func")
  (copilot-mode)
    (use-local-map copilot-completion-map))

(use-package copilot
  :ensure    t
  :defer     nil
  :hook      (prog-mode . cow-copilot-mode-hook-func)
  :custom    (copilot-idle-delay 2))       ; <--- less distractions

(define-key copilot-completion-map (kbd "C-S-<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-s-<tab>") 'copilot-accept-completion-by-word)
;; 
(define-key copilot-completion-map (kbd "C-S-n")     'copilot-next-completion)
(define-key copilot-completion-map (kbd "C-S-p")     'copilot-previous-completion)

(unless (copilot-installed-version)
  (copilot-install-server))

(provide 'cow-github-copilot)
