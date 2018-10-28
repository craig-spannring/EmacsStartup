
(defun install-and-require-packages (packages)
  "Require all packages in the list, installing any that are missing

PACKAGES is a list of packages that are required.
"
  (mapc (lambda (pkg) 
	(let* (;; f is the function that tells emacs the package is required.
	       (f (lambda () (progn (require pkg)))))
	  (condition-case nil (funcall f) ;; Attempt to "require" the package
	    (error (progn (condition-case nil
			      (progn
				;; If the package wasn't found catch the error.
				;; To handle the error we'll try to install and 
				;; then make a 2nd attempt at requiring it
				(message "Installing %s" pkg)
				(package-install pkg)
				(funcall f))
			    (error
                             (message "Couldn't load %s" pkg))))))))
        packages))


(setenv "PAGER" "cat")
(setq load-path (cons (expand-file-name "~/.elisp-redo2018") load-path))
(when (equal system-name "bznmac02.local")
  ;; TODO we should add /usr/local/bin iff it isn't already in the path
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))
 
(require 'dired-x)
(load "setup_svn")
(load "setup_rtags")
(load "setup_moon")
(load "msvc_functions")
(load "setup_cmake")
(load "setup_stuff_for_cc_mode")
(load "setup_dired")
(load "setup_qt")
(load "setup_other_global_settings")
(load "setup_global_keys")
(load "setup_server")


