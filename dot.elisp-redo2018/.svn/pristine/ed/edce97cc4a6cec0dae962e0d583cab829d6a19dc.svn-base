
;; (condition-case nil
;;   (progn 
;;     (cond ((equal subversion-preferred-package 'dsvn)
;;            (message "Setting up for dsvn")
;;            (autoload 'svn-status "dsvn" "Run `svn status'." t)
;;            (autoload 'svn-update "dsvn" "Run `svn update'." t)
;;            (require 'vc-svn))
;;           (t 
;;            ;; (require 'psvn))))
;;            (autoload 'svn-status "psvn" "Run `svn status'." t))))

(condition-case nil
  (progn 
    (cond ((equal subversion-preferred-package 'dsvn)
           (message "Setting up for dsvn")
	   (if (< emacs-major-version 25)
		  (add-hook 'log-edit-hook 'teejet-log-edit-add-summary))
           (add-hook 'log-edit-hook 'svn_bz-insert-bugzilla-synopsis)
           (require 'dsvn))
          (t 
           (message "Setting up for psvn")
           (require 'psvn)))))

(require 'vc-svn)

