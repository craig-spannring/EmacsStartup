
(when (not (string-match "XEmacs" emacs-version))
  (autoload 'python-mode "python-mode" "Python editing mode." t)
  
  (setq auto-mode-alist 
        (append 
         (list '("\\.py\\'" . python-mode)) auto-mode-alist))
  (setq interpreter-mode-alist
        (cons '("python" . python-mode)
              interpreter-mode-alist))

)
