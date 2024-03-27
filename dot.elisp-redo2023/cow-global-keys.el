;;; Setup keybindings.

(global-set-key "\C-cf" 'find-dired)
(global-set-key "\C-cn" 'find-name-dired)
(global-set-key "\C-cl" 'find-grep-dired)

(global-set-key [f12]   'compare-windows)
(global-set-key [f6]    'other-window) 
(global-set-key [f9 ?c] 'cow-compile-project)
(global-set-key [f9 f9] 'next-error)
(global-set-key [f5 ?c] 'compile)



(cond (nil ;; For now, don't use this next set of mappings. 
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
	  ;; Special keyboard setup for a Mac.
          (define-key c++-mode-map [s-right] 'gud-next)
          (define-key c++-mode-map [s-down]  'gud-step)
          (define-key c++-mode-map [s-up]    'gud-finish)
          (define-key c-mode-map [s-right]   'gud-next)
          (define-key c-mode-map [s-down]    'gud-step)
          (define-key c-mode-map [s-up]      'gud-finish)
          (define-key gud-mode-map [s-right] 'gud-next)
          (define-key gud-mode-map [s-down]  'gud-step)
          (define-key gud-mode-map [s-up]    'gud-finish))))

(provide 'cow-global-keys)
