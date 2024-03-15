;;; Setup to use DSVN access to Subversion
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
  :group 'cts-emacs-conf)


 (add-hook 'log-edit-hook 'svn_bz-insert-bugzilla-synopsis)


(condition-case nil
  (progn 
    (cond ((equal subversion-preferred-package 'dsvn)
           (message "Setting up for dsvn")
           ;; (add-hook 'log-edit-hook 'svn_bz-insert-bugzilla-synopsis)
           (require 'dsvn))
          (t 
           (message "Setting up for psvn")
           (require 'psvn)))))

(require 'vc-svn)

(provide 'subversion-cts)
