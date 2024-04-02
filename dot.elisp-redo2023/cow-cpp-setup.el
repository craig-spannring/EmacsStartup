;;; This sets up a C++ IDE
;;;
;;; See README.md for more details.
;;;
;;; Desired Capabilities
;;;  1. auto-complete symbols
;;;  2. Ability to jump to file using basename instead of requiring
;;;     user to know full path.
;;;  3. Determine datatype of symbol under the cursor
;;;  4. Find references to arbitrary symbol
;;;  5. Find references to symbol under the cursor
;;;  6. Find declarations of arbitrary symbol
;;;  7. Find declaration/definition of symbol under cursor
;;;  8. cd into project directory and run compile
;;;  9. Flip between source and header file
;;; 10. rename symbol
;;; 11. show inheritance tree.
;;; 12.  find reimplementations of virthual method underneath cursor.

(message "Loading C++ support.")

(require 'cow-package-helpers)
(require 'cow-complete-common)
(require 'cow-lsp-common)

(defcustom cow-cpp-support 'use-rtags-cpp
  "Select which Emacs package to use for C++
Note- Changes won't take effect until you restart emacs."
  :type '(choice
	  (const use-lsp-cpp)
	  (const use-rtags-cpp))
  :group 'cow-emacs-conf)

;; Setup the selected style of C++ support
(cond
 ((equal cow-cpp-support 'use-rtags-cpp)
  (message "Using rdm for C++ mode")
  (require 'cow-rtags-common)
  (add-hook 'c-mode-common-hook 'company-mode))
 ((equal cow-which-lsp-package 'use-lsp)
  (message "Using lsp-mode for C++ mode.")
  (require 'cow-lsp-clangd-common)
  (use-package yasnippet :ensure t)
  (use-package lsp-mode
               :ensure   t
               ;; :config   (setq lsp-auto-guess-root t) ; Automatically guess project root
               :commands (lsp lsp-deferred)
               :hook ((c-mode-hook   lsp)
                      (c++-mode . lsp-deferred))
               :config   (setq lsp-log-io t)
               :config   (setq lsp-auto-configure t)))
 ((equal cow-which-lsp-package 'use-eglot)
    (message "Using eglot for C++ mode.")
    (install-and-require-packages '(eglot)) ;; todo- replace with use-package
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

    (add-hook 'c-mode-common-hook 'company-mode)
    (add-hook 'c-mode-common-hook 'eglot-ensure)))


(provide 'cow-cpp-setup)
