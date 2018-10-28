

(cond 
 ((string-match "XEmacs" emacs-version)
  nil)
 ((>=  emacs-major-version 23)
  nil)
 ((>=  emacs-major-version 22)
  (cond (window-system
         (setq hilit-mode-enable-list '(not text-mode))
         (setq hilit-background-mode   'light)
         (setq hilit-inhibit-hooks     nil)
         (setq hilit-inhibit-rebinding nil)
         ;(require 'hilit19)
         ))
  (setq hilit-auto-highlight-maxout 400000)
))

(require (quote font-lock)) 

(cond (window-system (font-lock-mode 1))
      (t             (font-lock-mode 0)))





;;
;; Add VOS command macros to the syntax hilit19 stuff
;;
(setq auto-mode-alist 
      (append 
       (list '("\\.cm\\'" . vos-cm-mode))
       auto-mode-alist))

