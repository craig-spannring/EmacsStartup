
(require 'python)

(add-hook 'python-mode-hook
          #'(lambda ()
              (setq python-indent-offset 4)))

;;       (define-key python-mode-map "\C-m" 'newline-and-indent)))
