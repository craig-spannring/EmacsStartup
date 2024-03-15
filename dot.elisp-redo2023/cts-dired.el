;; Bring in dired and setup some global keybinds 
(require 'dired-x)
(global-set-key "\C-cf" 'find-dired)
(global-set-key "\C-cn" 'find-name-dired)
(global-set-key "\C-cl" 'find-grep-dired)

(provide 'cts-dired)
