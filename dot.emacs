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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ada-clean-buffer-before-saving nil)
 '(ada-label-indent 0)
 '(ada-search-directories (quote ("." "$ADA_INCLUDE_PATH")))
 '(comint-password-prompt-regexp
   "\\(\\([Oo]ld \\|[Nn]ew \\|'s \\|login \\|Kerberos \\|CVS \\|UNIX \\| SMB \\|\\[sudo\\] \\|^\\)[Pp]assword\\( (again)\\)?\\|pass phrase\\|\\(Enter\\|Repeat\\|Bad\\) passphrase\\)\\(?:, try again\\)?\\(?: for [^:]+\\)?:\\s *\\'")
 '(company_name "TeeJet Technologies")
 '(current-coding-standard "MI_STYLE")
 '(cvs-buffer-name-alist
   (quote
    (("diff" cvs-diff-buffer-name diff-mode)
     ("status" "*cvs-info*" cvs-status-mode)
     ("tree" "*cvs-info*" cvs-status-mode)
     ("message" "*cvs-commit*" nil cvs-log-edit-insert-pr-summary)
     ("log" "*cvs-info*" log-view-mode))))
 '(cvs-edit-hook
   (quote
    (cvs-edit-insert-cvs-template cvs-edit-insert-changelog)))
 '(cvs-edit-mode-hook (quote (cvs-insert-pr-summary)))
 '(cvs-mode-commit-hook (quote (cvs-insert-pr-summary)))
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
 '(display-time-format nil)
 '(display-time-mode t)
 '(fancy-splash-image "~/.elisp/teejetlogo.jpeg")
 '(find-grep-options "-I -q")
 '(graphviz-dot-auto-indent-on-semi nil)
 '(ispell-program-name "aspell")
 '(jabber-backlog-days 30.0)
 '(jabber-history-enable-rotation t)
 '(jabber-history-enabled t)
 '(jabber-history-size-limit 10240)
 '(line-move-visual nil)
 '(mediawiki-site-alist
   (quote
    (("TeeJetWiki" "http://bznwiki.spray.com/MediaWiki/mediawiki-current/" "cspannri" "MIclb37diu." "SSCO_HQ" "Main Page"))))
 '(mouse-drag-copy-region t)
 '(name_of_coder "Craig Spannring")
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (s define-word cmake-ide my-package-that-does-not-exist qwerty-foo-bar cmake-mode company)))
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


