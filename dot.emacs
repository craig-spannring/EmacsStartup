;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/")
	                  '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(cond
 ((= 25 emacs-major-version) (load "~/.elisp-redo2018/emacs-stage2.el"))
 (t                          (load "~/.elisp/emacs-stage2.el")))

(setq software_version_string "")


;; TODO Think about how we want to handle customization
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company_name "Montana Instruments")
 '(current-coding-standard "MI_STYLE")
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
 '(display-time-format nil)
 '(display-time-mode t)
 '(fancy-splash-image "~/.elisp/Logo_MI_coldscience.jpeg")
 '(graphviz-dot-auto-indent-on-semi nil)
 '(mediawiki-site-alist
   (quote
    (("MI" "http://miwiki.mti.local/wiki" "craig.spannring" "" nil "Main Page"))))
 '(mouse-drag-copy-region t)
 '(name_of_coder "Craig Spannring")
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (rtags mediawiki magit s define-word cmake-ide cmake-mode company)))
 '(save-place t nil (saveplace))
 '(select-enable-clipboard nil)
 '(select-enable-primary t)
 '(show-paren-mode t)
 '(split-width-threshold 200)
 '(subversion-preferred-package (quote dsvn))
 '(tool-bar-mode t)
 '(undo-outer-limit 5000000)
 '(w3m-home-page "http://bzncode.spray.com")
 '(which-func-maxout 0)
 '(which-func-mode-global t nil (which-func))
 '(which-func-non-auto-modes nil))

;; Matlab editing support
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


