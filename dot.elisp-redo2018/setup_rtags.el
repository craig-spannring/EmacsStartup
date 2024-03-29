
(install-and-require-packages '(rtags))
(install-and-require-packages '(company-rtags))
;; (install-and-require-packages '(company))
(install-and-require-packages '(flycheck))
(install-and-require-packages '(flycheck-rtags))
(install-and-require-packages '(popup))
	


;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))
  

;; only run this if rtags is installed
(when (require 'rtags nil :noerror)
  ;; make sure you have company-mode installed
  (require 'company)
  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  ;;;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
  ;;;;  (define-key prelude-mode-map (kbd "C-c r") nil)
  ;; install standard rtags keybindings. Do M-. on the symbol below to
  ;; jump to definition and see the keybindings.
  (rtags-enable-standard-keybindings)
  ;; comment this out if you don't have or don't use helm
;  (setq rtags-use-helm t)
  ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;; use rtags flycheck mode -- clang warnings shown inline
;  (require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
  ;; start the rdm server if needed when we edit C/C++ code. 
  ;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  ;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  ;; (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  )
