;; Some common definitions for LSP 
;;
;; 

(defcustom cts-which-lsp-package  'use-eglot
  "Which type of python support do we want to use?"
  :type '(choice
          (const use-eglot) ; see https://www.gnu.org/software/emacs/manual/html_mono/eglot.html
          (const use-lsp))  ; 
  :group 'cts-emacs-conf)

(cond
 ((equal cts-which-lsp-package 'use-lsp)
  (install-and-require-packages '(lsp-mode lsp-ui))
  (setq lsp-keymap-prefix "s-l"))
 ((equal cts-which-lsp-package 'use-eglot)
  (install-and-require-packages '(eglot))))

(provide 'lsp-common-cts)
