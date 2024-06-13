;;;
;;; Setup keybindings for C/C++ mode

(require 'cow-cpp-setup)

;; (define-key c-mode-map   "\C-co"    'msvc-find-other-file)
;; (define-key c++-mode-map "\C-co"    'msvc-find-other-file)

(cond
 ((eq cow-cpp-support 'use-lsp-cpp)
  ;; todo setup any keys we might want for LSP  
  nil)
 ((eq cow-cpp-support 'use-rtags-cpp)
  ;; todo setup any keys we might want for RTAGS
  nil))
  

(provide 'cow-keys-cpp)
