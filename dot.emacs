; -*- mode: emacs-lisp -*-



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/")
	                  '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(setq use-redo2023 nil)

(let (
      (cts-stage2      (cond
                        (use-redo2023               (expand-file-name "~/.elisp-redo2023/emacs-stage2.el"))
                        ((> emacs-major-version 28) (expand-file-name (format "~/.elisp%d/emacs-stage2.el"
                                                                              emacs-major-version)))
                        ((> emacs-major-version 24) (expand-file-name "~/.elisp-redo2018/emacs-stage2.el"))
                        (t                          (expand-file-name "~/.elisp/emacs-stage2.el")))))


  (if (not (file-exists-p cts-stage2))
      (message "Warning: Could not find stage2 file %s" cts-stage2)
    (load cts-stage2)))
        
;;  
;;  ;; Matlab editing support
;;  (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;;  (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;;  (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
;;  
;;  (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;   )
;;  

 ; '(safe-local-variable-values
 ;   (quote
 ;    ((eval c-set-offset
 ;           (quote arglist-cont-nonempty)
 ;           (quote
 ;            (c-lineup-gcc-asm-reg c-lineup-arglist)))
 ;     (eval c-set-offset
 ;           (quote arglist-close)
 ;           0)
 ;     (eval c-set-offset
 ;           (quote arglist-intro)
 ;           (quote ++))
 ;     (eval c-set-offset
 ;           (quote case-label)
 ;           0)
 ;     (eval c-set-offset
 ;           (quote statement-case-open)
 ;           0)
 ;     (eval c-set-offset
 ;           (quote substatement-open)
 ;           0))))

;; (put 'upcase-region 'disabled nil)

