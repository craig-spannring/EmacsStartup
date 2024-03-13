;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/")
	                  '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(let ((cts-custom-file (expand-file-name
                        (format "%s/custom-%d.el" user-emacs-directory emacs-major-version)))
      (cts-stage2      (cond
                        ((> emacs-major-version 28) (expand-file-name (format "~/.elisp%d/emacs-stage2.el"
                                                                              emacs-major-version)))
                        ((> emacs-major-version 24) (expand-file-name "~/.elisp-redo2018/emacs-stage2.el"))
                        (t                          (expand-file-name "~/.elisp/emacs-stage2.el")))))


  (setq custom-file cts-custom-file)
  (if (file-exists-p custom-file) (load custom-file))
      
  (if (not (file-exists-p cts-stage2))
      (message "Warning: Could not find stage2 file %s" cts-stage2)
    (load cts-stage2)))
        
 
(setq software_version_string "")
;; (set-face-attribute 'default nil :height 90)
(transient-mark-mode 0)

;;  
;;  ;; TODO Think about how we want to handle customization
;;  (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;   '(ansi-color-faces-vector
;;     [default default default italic underline success warning error])
;;   '(ansi-color-names-vector
;;     ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
;;   '(company_name "Montana Instruments")
;;   '(cts-c-file-header-style "MI_STYLE")
;;   '(current-coding-standard "MI_STYLE")
;;   '(custom-enabled-themes nil)
;;   '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
;;   '(display-time-format nil)
;;   '(display-time-mode t)
;;   '(fancy-splash-image "~/.elisp/Logo_MI_coldscience.jpeg")
;;   '(find-grep-options "-q --exclude '*.svn-base'")
;;   '(graphviz-dot-auto-indent-on-semi nil)
;;   '(gud-pdb-command-name "pdb3")
;;   '(line-move-visual nil)
;;   '(mediawiki-site-alist
;;     (quote
;;      (("MI" "http://miwiki.mti.local/wiki" "craig.spannring" "" nil "Main Page"))))
;;   '(mouse-drag-copy-region t)
;;   '(name_of_coder "Craig Spannring")
;;   '(package-archives
;;     (quote
;;      (("gnu" . "http://elpa.gnu.org/packages/")
;;       ("melpa" . "https://melpa.org/packages/")
;;       ("melpa-stable" . "https://stable.melpa.org/packages/"))))
;;   '(package-selected-packages
;;     (quote
;;      (realgud-lldb string-inflection json-mode ztree sql-indent lorem-ipsum csharp-mode org-jira modern-cpp-font-lock popup mediawiki magit s define-word cmake-ide cmake-mode company)))
;;   '(python-shell-interpreter "python3")
;;   '(save-place-mode t nil (saveplace))
;;   '(scroll-bar-mode (quote right))
;;   '(select-enable-clipboard nil)
;;   '(select-enable-primary t)
;;   '(show-paren-mode t)
;;   '(split-width-threshold 200)
;;   '(sql-ms-program "sqlcmd")
;;   '(subversion-preferred-package (quote dsvn))
;;   '(tool-bar-mode t)
;;   '(undo-outer-limit 5000000)
;;   '(w3m-home-page "http://bzncode.spray.com")
;;   '(which-func-maxout 0)
;;   '(which-func-mode-global t nil (which-func))
;;   '(which-func-non-auto-modes nil))
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
