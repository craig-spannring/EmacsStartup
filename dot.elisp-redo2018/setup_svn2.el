

(condition-case nil
  (progn 
    (cond ((equal subversion-preferred-package 'dsvn)
           (message "Setting up for dsvn")
           (add-hook 'log-edit-hook 'svn_bz-insert-bugzilla-synopsis)
           (require 'dsvn))
          (t 
           (message "Setting up for psvn")
           (require 'psvn)))))

(require 'vc-svn)

