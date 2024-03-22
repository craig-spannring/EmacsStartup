;;; Setup keybindings.


(global-set-key [f12] 'compare-windows)
(global-set-key [f6] 'other-window) 

(cond (nil
        ;;
        ;; setup keys to make it easier to step in gdb
        ;; 
        (define-key c++-mode-map [kp-right] 'gud-next)
        (define-key c++-mode-map [kp-down]  'gud-step)
        (define-key c++-mode-map [kp-up]    'gud-finish)
        (define-key c-mode-map   [kp-right] 'gud-next)
        (define-key c-mode-map   [kp-down]  'gud-step)
        (define-key c-mode-map   [kp-up]    'gud-finish)
        (define-key gud-mode-map [kp-right] 'gud-next)
        (define-key gud-mode-map [kp-down]  'gud-step)
        (define-key gud-mode-map [kp-up]    'gud-finish)
        (when (equal system-type 'darwin)
          (define-key c++-mode-map [s-right] 'gud-next)
          (define-key c++-mode-map [s-down]  'gud-step)
          (define-key c++-mode-map [s-up]    'gud-finish)
          (define-key c-mode-map [s-right]   'gud-next)
          (define-key c-mode-map [s-down]    'gud-step)
          (define-key c-mode-map [s-up]      'gud-finish)
          (define-key gud-mode-map [s-right] 'gud-next)
          (define-key gud-mode-map [s-down]  'gud-step)
          (define-key gud-mode-map [s-up]    'gud-finish))))

(global-set-key [f9 ?c] 'cow-compile-project)

(provide 'cow-global-keys)
