;;; Setup to use version control
;;;
;;; TODO- Figure out what hooks we still want.  Prior to the 2023 remake 
;;;       we had one hook, `svn_bz-insert-bugzilla-synopsis` which
;;;       would grab the synopsis for a bugzilla issue and place it
;;;       in the log message. 
;;; 

(defcustom subversion-preferred-package 'dsvn
  "Select which Emacs package to use for Subversion. 
Note- Changes won't take effect until you restart emacs."
  :type '(choice (const psvn) (const dsvn))
  :group 'cow-emacs-conf)

;; (add-hook 'log-edit-hook 'svn_bz-insert-bugzilla-synopsis)



(condition-case nil
    (progn 
      (cond ((equal subversion-preferred-package 'dsvn)
             (message "Setting up for dsvn")
             ;; (add-hook 'log-edit-hook 'svn_bz-insert-bugzilla-synopsis)
             (condition-case nil
                 (require 'dsvn)
               (file-missing
                (let* ((fname (concat (file-name-directory load-file-name) "3rdParty/dsvn.el")))
                  (load fname))
                (require 'dsvn)
                ))
             )
            (t 
             (message "Setting up for psvn")
             (require 'psvn)))))

(require 'vc-svn)

(use-package magit
 :ensure t)

(provide 'cow-vc)
