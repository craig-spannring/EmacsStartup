
(require 'setup_w3)

(require 'tempo)
(setq load-path (cons '"~/.elisp/doxymacs-1.6.0/no-autoconf" load-path))
(require 'doxymacs)

(add-hook 'c-mode-common-hook 'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode)
          (eq major-mode 'c++-mode)
          (eq major-mode 'text-mode))
      (doxymacs-font-lock)))

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
