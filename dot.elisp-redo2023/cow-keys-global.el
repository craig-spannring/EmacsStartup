;;; Setup keybindings.

(global-set-key [f12]   'compare-windows)
(global-set-key [f6]    'other-window) 
(global-set-key [f9 ?c] 'cow-compile-project)
(global-set-key [f9 f9] 'next-error)
(global-set-key [f5 ?c] 'compile)


(provide 'cow-keys-global)
