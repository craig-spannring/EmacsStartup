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

(require 'package-helpers-cts)
(require 'complete-common-cts)

;; (global-set-key [f9 ?f] 'msvc-find-file)
;; (global-set-key [f9 ?c] 'msvc-compile-current-project)
;; (global-set-key [f9 f9] 'next-error)
;; (global-set-key [f5 ?c] 'compile)

(cond
 ((equal cts-which-lsp-package 'use-lsp)
  (message "Using lsp-mode for C++ mode.")
  ;; (install-and-require-packages '(lsp-mode))
  ;; (use-package lsp-mode
  ;;              :ensure t
  ;;              :hook ((c-mode-hook   lsp)
  ;;                     (c++-mode-hook lsp)
  ;;                     (c++-mode-hook myhook))
  ;;              )
  (use-package yasnippet :ensure t)
  (use-package lsp-mode
               :ensure   t
               :config   (setq lsp-auto-guess-root t) ; Automatically guess project root
               :commands (lsp lsp-deferred)
               :hook ((c-mode-hook   lsp)
                      (c++-mode . lsp-deferred))
               :config   (setq lsp-log-io t)
               :config   (setq lsp-auto-configure t))
  )
 ((equal cts-which-lsp-package 'use-eglot)
    (message "Using eglot for C++ mode.")
    (install-and-require-packages '(eglot))
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
