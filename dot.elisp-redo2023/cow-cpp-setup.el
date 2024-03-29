;;; This sets up a C++ IDE
;;;
;;; Capabilities 
;;;  1. auto-complete symbols
;;;  2. (todo) Ability to jump to file using basename instead of requiring
;;;            user to know full path
;;;  3. (todo) Determine datatype of symbol under the cursor
;;;  4. (todo) Find references to arbitrary symbol 
;;;  5. (todo) Find references to symbol under the cursor
;;;  6. (todo) Find declarations of arbitrary symbol
;;;  7. (todo) Find declaration/definition of symbol under cursor
;;;  8. (todo) cd into project directory and run compile
;;;  9. (todo) Flip between source and header file
;;; 10. (todo) rename symbol
;;; 11. (todo) show inheritance tree.
;;; 12  (todo) find reimplementations of virthual method underneath cursor.
;;; 


(message "Loading C++ support.")

(require 'cow-package-helpers)
(require 'cow-complete-common)

(defcustom cow-cpp-support 'use-rtags-cpp
  "Select which Emacs package to use for C++ 
Note- Changes won't take effect until you restart emacs."
  :type '(choice (const use-lsp-cpp) (const use-rtags-cpp))
  :group 'cow-emacs-conf)

;; Setup the selected style of C++ support
(cond
 ((equal cow-cpp-support 'use-rtags-cpp)
  (message "Using rdm for C++ mode")
  (when (< emacs-major-version 29)
    (use-package rtags
      :ensure t
      :config (setq rtags-completions-enabled t)
      :config (setq rtags-use_helm            t)
      :config (setq rtags-display-result-backend      'helm))
    (use-package company-rtags  :ensure t)
    (use-package flycheck-rtags :ensure t)))
 ((equal cts-which-lsp-package 'use-lsp)
  (message "Using lsp-mode for C++ mode.")
  (use-package yasnippet :ensure t)
  (use-package lsp-mode
               :ensure   t
               :config   (setq lsp-auto-guess-root t) ; Automatically guess project root
               :commands (lsp lsp-deferred)
               :hook ((c-mode-hook   lsp)
                      (c++-mode . lsp-deferred))
               :config   (setq lsp-log-io t)
               :config   (setq lsp-auto-configure t)))
 ((equal cts-which-lsp-package 'use-eglot)
    (message "Using eglot for C++ mode.")
    (install-and-require-packages '(eglot)) ;; todo- replace with use-package
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

    (add-hook 'c-mode-common-hook 'company-mode)
    (add-hook 'c-mode-common-hook 'eglot-ensure)))


(provide 'cow-cpp-setup)
