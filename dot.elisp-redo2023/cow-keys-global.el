;;; Setup keybindings.

(global-set-key [f12]   'compare-windows)
(global-set-key [f6]    'other-window) 
(global-set-key [f9 ?c] 'cow-compile-project)
(global-set-key [f9 f9] 'next-error)
(global-set-key [f5 ?c] 'compile)

(when (equal system-type 'darwin)
  ;; Special keyboard setup for a Mac.
  (global-set-key [home] 'move-beginning-of-line)
  (global-set-key [end] 'move-end-of-line))

(provide 'cow-keys-global)
