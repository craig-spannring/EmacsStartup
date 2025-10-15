;;; Routines that help with using Emacs' package management. 


(message "Loading package helpers")

(defun install-and-require-packages (packages)
  "Require all packages in the list, installing any that are missing

PACKAGES is a list of packages that are required.

e.g.
   (install-and-require-packages '(string-inflection sql-indent))

Note: This could likely be replaced by
  (use-package pkg :ensure t)
"
  (mapc
   (lambda (pkg) 
     (let* (;; f is the closure function to tell emacs the package is required.
            (f (lambda () (progn (require pkg)))))
       (condition-case nil (funcall f) ;; Attempt to "require" the package
	 (error (progn (condition-case nil
			   (progn
			     ;; If the package wasn't found catch the error.
			     ;; To handle the error we'll try to install and 
			     ;; then make a 2nd attempt at requiring it
			     (message "Installing %s" pkg)
                             (package-refresh-contents)
			     (package-install pkg)
			     (funcall f))
			 (error
                          (message "Couldn't load %s" pkg)
                          (sleep-for 2.0))))))))
   packages))

(install-and-require-packages '(use-package))

(provide 'cow-package-helpers)
