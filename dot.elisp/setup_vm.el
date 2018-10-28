;;
;;  Add the View Mail stuff
;;
(cond 
 ((not (or 
        (equal system-type 'windows-nt)
	(equal system-type 'emx)
	(string= (system-name) "olympus.bzn.vlt.eds.com")))
  (require 'vm)
  (require 'mailcrypt)
  (autoload 'vm "vm" "Start VM on your primary inbox." t)
  (autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
  (autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
  (autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
  (autoload 'vm-mail "vm" "Send a mail message using VM." t)
  (autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)
  (setq vm-auto-folder-case-fold-search t)
  (setq mail-archive-file-name "~/outbox")
  (setq vm-confirm-new-folders t)
  (setq vm-folder-directory "~/MailFolders/")
  (setq vm-visit-when-saving t)
  (setq vm-delete-after-saving t)

  (load "setup_vm_folderlist");

  (setq mail-archive-file-name "~/outbox")
  (setq vm-confirm-new-folders t)
  (setq vm-folder-directory "~/MailFolders/")

  (load "setup_vm_spoolfiles");

  (setq vm-visit-when-saving t)
  (setq vm-delete-after-saving t)
  (setq vm-delete-after-archiving t)
  (setq vm-auto-get-new-mail 120)
  (setq vm-mail-header-from "Craig Spannring <craig@spannring.org>")
  (setq vm-reply-subject-prefix "Re: ")

  (setq vm-font-lock-words
        '(("^Subject: \\(.*\\)$" . font-lock-type-face)
          ("^Sender: \\(.*\\)$" . font-lock-reference-face)
          ("^From: \\(.*\\)" . font-lock-string-face)
          ("^To: \\(.*\\)" . font-lock-keyword-face)
          ("^CC: \\(.*\\)" . font-lock-keyword-face)
          ("^cc: \\(.*\\)" . font-lock-keyword-face)
          ("^Date: \\(.*\\)" . font-lock-reference-face)
          ("^[a-zA-Z \t]*[>|}].*[>|}].*[>|}].*$" 
           . font-lock-variable-name-face)
          ("^[a-zA-Z \t]*[>|}].*[>|}].*$" . font-lock-reference-face) 
          ("^[a-zA-Z \t]*[>|}].*" . font-lock-comment-face)
          ("^.*\\\[Click .*\\\]$" . font-lock-variable-name-face)
          ("\\(file\\|ftp\\|gopher\\|http\\|https\\|news\\|wais\\|www\\)://[^
\t\n\f\r\"|()]*[^ \t\n\f\r\"|.!?(){}]" . font-lock-string-face)
          )
        )

  (defun vm-fontify ()
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(vm-font-lock-words t))
    (turn-on-font-lock))

  (cond (window-system
         (add-hook 'vm-mode-hook
                   '(lambda ()
                      (local-set-key "r" 'vm-followup)
                      (vm-fontify)))
         ))

  (add-hook 'vm-mode-hook         'mc-install-read-mode)
  (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-mail-mode-hook    'mc-install-write-mode)
  
  
  (add-hook 'vm-mail-mode-hook '(lambda () (auto-fill-mode 1)))
))

(setq mail-signature t)
(setq mail-yank-prefix ">  ")

