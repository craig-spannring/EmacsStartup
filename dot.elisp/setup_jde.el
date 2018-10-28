
(setq load-path (cons '"~/.elisp/jde" load-path))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun my-java-mode-hook ()
  (cond 
   ((and 
     window-system 
     (not (and (boundp 'running-xemacs) running-xemacs)))
    (require 'andersl-java-font-lock)
    (turn-on-font-lock))))

(setq font-lock-maximum-decoration t)

(require 'jde)