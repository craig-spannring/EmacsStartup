
;(when (string= system-name "segovia.local")

(condition-case error
  (load "~/.elisp/haskell-mode-2.4/haskell-site-file")
  (error nil))
;)


; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)