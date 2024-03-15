;;; This sets up a C++ IDE
;;;

(message "Loading C++ support.")

(require 'package-helpers-cts)
(require 'complete-common-cts)

;; (global-set-key [f9 ?f] 'msvc-find-file)
;; (global-set-key [f9 ?c] 'msvc-compile-current-project)
;; (global-set-key [f9 f9] 'next-error)
;; (global-set-key [f5 ?c] 'compile)

(cond
 ((equal cts-which-lsp-package 'use-lsp)
  (message "We don't support lsp-mode for C++ yet."))
 ((equal cts-which-lsp-package 'use-eglot)
    (install-and-require-packages '(eglot))
    (message "Using eglot for C++ mode.")
    ;; (use-package eglot
    ;;              :ensure t
    ;;              :config
    ;;              (add-to-list 'eglot-server-programs '(cc-mode  . ("clangd")))
    ;;              (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
    ;; 
    ;;              :hook ((c-mode-hook   . eglot-ensure)
    ;;                     (c++-mode-hook . eglot-ensure)))
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

    (mapc (lambda (x)
            (add-hook 'c-mode-hook   x)
            (add-hook 'c++-mode-hook x))
          '(company-mode
            eglot-ensure))
    ))


(provide 'cpp-setup-cts)
