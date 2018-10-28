;;
;;  Add in the gnats bug tracking system
;;
(setq load-path (cons '"/usr/local/lib/emacs/lisp" load-path))
(autoload 'edit-pr "gnats" "Command to edit a problem report." t)
(autoload 'view-pr "gnats" "Command to view a problem report." t)
(autoload 'unlock-pr "gnats" "Unlock a problem report." t)
(autoload 'query-pr "gnats" "Command to query information about problem reports." t)
(autoload 'send-pr-mode "send-pr" "Major mode for sending problem reports." t)
(autoload 'send-pr "send-pr" "Command to create and send a problem report." t)
;; (setq gnats:root "/usr/local/lib/gnats/gnats-db")
;; (setq gnats:libdir "/usr/local/lib")


