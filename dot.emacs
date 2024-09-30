; -*- mode: emacs-lisp -*-

;; (setq user-emacs-directory
;;       (expand-file-name
;;        (format "~/.emacs.d-%d.el"
;;                emacs-major-version)))
(message "user-emacs-directory is %s" user-emacs-directory)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("gnu"          . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
;; (add-to-list 'package-archives '("org"          . "https://orgmode.org/elpa/"))
             
(package-initialize)

(if (< emacs-major-version 24) (error "Requires Emacs 24 or newer"))

(defconst cow-setup-era
  (let* ((era (getenv "COW_SETUP_ERA")))
    (cond ((string-equal era "ancient") 'ancient)
          ((string-equal era "2018")    'v2018)
          ((string-equal era "2023")    'v2023)
          ((null era)                   'v2023)
          (t                            (error "Invalid COW_SETUP_ERA.  Must be ancient, 2018, or 2023"))))
  "Select which era of emacs initialization to use. ")

(let ((cts-stage2
       (cond
        ((equal cow-setup-era 'v2023)   (expand-file-name "~/.elisp-redo2023/emacs-stage2.el"))
        ((equal cow-setup-era 'v2018)   (expand-file-name "~/.elisp-redo2018/emacs-stage2.el"))
        ((equal cow-setup-era 'ancient) (expand-file-name "~/.elisp/emacs-stage2.el"))
        ((< emacs-major-version 25)     (expand-file-name "~/.elisp-redo2018/emacs-stage2.el"))
        (t                              (expand-file-name "~/.elisp/emacs-stage2.el")))))
  (if (not (file-exists-p cts-stage2))
      (message "Warning: Could not find stage2 file %s" cts-stage2)
    (load cts-stage2)))
