(require 'ada-mode)

(when (not (xemacs-p))
  (when (or (not (boundp 'ada-xemacs)) (not ada-xemacs))
    (global-font-lock-mode t)
    (cond 
     ((< emacs-major-version 23)
      (add-hook 'find-file-hooks 'turn-on-font-lock-if-enabled)))))
