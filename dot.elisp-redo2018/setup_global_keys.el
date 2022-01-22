;;; $Header: /home/cts/PersonalRepository.tmp/.elisp/setup_global_keys.el,v 1.19 2011/11/27 19:28:48 cts Exp $

(require 'gud)

;;
;; global key settings for Emacs.
;;
(global-set-key "\M-g"  'goto-line)
(global-set-key [f6] 'other-window) 
;; (global-set-key [f7] 'find-tag)
;; (global-set-key [f8] '_find_next_tag)

(global-set-key [f9 ?f] 'msvc-find-file)
(global-set-key [f9 ?c] 'msvc-compile-current-project)
(global-set-key [f9 f9] 'next-error)
(global-set-key [f5 ?c] 'compile)


;;
;;
;; bind some keys for the dired find functions
;;
(global-set-key "\C-cf" 'find-dired)
(global-set-key "\C-cn" 'find-name-dired)
(global-set-key "\C-cl" 'find-grep-dired)

(when (equal system-type 'windows-nt)
  (setq compile-command "nmake "))
  
;; (cond ((not (and (boundp 'running-xemacs) running-xemacs))
;;        (global-set-key [f9 ?i] 'add_interruption)
;;        (global-set-key [f9 ?m] 'emx-match-paren)))

(global-set-key [f12] 'compare-windows)

;; (global-set-key [f19] 'dabbrev-expand)

(global-set-key [f11]         'msvc-tags-find-declaration) ; 'ebrowse-tags-find-declaration)
(global-set-key [(shift f11)] 'msvc-tags-find-definition)  ; 'ebrowse-tags-find-definition) 

;;  (global-set-key [f13] 'next-error)
;;  (global-set-key [f3] 'next-error)
;; (global-set-key [(shift f3)] 'next-error)
;; (if (not (string-match "XEmacs" emacs-version))
;;     (global-set-key [S-f3] 'next-error))

(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)
;; (global-set-key [insertchar] 'overwrite-mode)
;; (global-set-key [delete] 'delete-char)

;;
;; setup keys to make it easier to step in gdb
;; 
(define-key c++-mode-map [kp-right] 'gud-next)
(define-key c++-mode-map [kp-down]  'gud-step)
(define-key c++-mode-map [kp-up]    'gud-finish)
(define-key c-mode-map   [kp-right] 'gud-next)
(define-key c-mode-map   [kp-down]  'gud-step)
(define-key c-mode-map   [kp-up]    'gud-finish)
(define-key gud-mode-map [kp-right] 'gud-next)
(define-key gud-mode-map [kp-down]  'gud-step)
(define-key gud-mode-map [kp-up]    'gud-finish)
(when (equal system-type 'darwin)
  (define-key c++-mode-map [s-right] 'gud-next)
  (define-key c++-mode-map [s-down]  'gud-step)
  (define-key c++-mode-map [s-up]    'gud-finish)
  (define-key c-mode-map [s-right]   'gud-next)
  (define-key c-mode-map [s-down]    'gud-step)
  (define-key c-mode-map [s-up]      'gud-finish)
  (define-key gud-mode-map [s-right] 'gud-next)
  (define-key gud-mode-map [s-down]  'gud-step)
  (define-key gud-mode-map [s-up]    'gud-finish))


;; (global-set-key [kp-right] 'gud-next)
;; (global-set-key [kp-down]  'gud-step)
;; (global-set-key [kp-up]    'gud-finish)

;; (when (equal system-type 'darwin)
;;   (global-set-key [s-right] 'gud-next)
;;   (global-set-key [s-down]  'gud-step)
;;   (global-set-key [s-up]    'gud-finish))

;; (when (equal system-type 'darwin)
;;   ; (setq mac-command-key-is-meta nil)
;;   ; (when (= emacs-major-version 22) (global-set-key mac-im-keystroke 'just-one-space))
;;   (setq mac-wheel-button-is-mouse-2 t)
;;   (global-set-key [kp-delete] 'delete-char))


;; (when (not (equal system-type 'windows-nt))
;;   (defun yank-from-x-cut-buffer (&optional which-one)
;;     (interactive)
;;     (when (not which-one) (setq which-one 0))
;;     (let ((text (x-get-cut-buffer which-one)))
;;       (when (and text (char-or-string-p text))
;;         (insert text)
;;         )
;;       )
;;     )
  
;;   (when (not (and (boundp 'running-xemacs) running-xemacs))
;;     (global-set-key [M-insert] 'yank-from-x-cut-buffer))
;; )
