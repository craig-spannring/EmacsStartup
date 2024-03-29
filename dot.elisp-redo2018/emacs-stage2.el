
(setq custom-file
      (let* ((tmp (expand-file-name
                   (format "%s/custom-%d-%s.el"
                           user-emacs-directory
                           emacs-major-version
                           cow-setup-era ))))
        (if (file-exists-p tmp) (load tmp))
        tmp))



(defvar cts-need-package-contents-refresh t)
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
                                (cond (cts-need-package-contents-refresh 
                                       (message "Refreshing package contents")
                                       (package-refresh-contents)
                                       (message "Refreshed package contents")
                                       (setq cts-need-package-contents-refresh nil)))
                                (sleep-for 0.1)
				(package-install pkg)
                                (message "Installed %s" pkg)
				(funcall f)
                                (message "Required %s" pkg))
			    (error
                             (message "Couldn't load %s" pkg))))))))
        packages))


(setenv "PAGER" "cat")
(setq load-path (cons (expand-file-name "~/.elisp-redo2018") load-path))
 
(require 'dired-x)
(load "setup_svn")
(load "setup_magit")

(if (version< emacs-version "28.0") 
    (load "setup_rtags"))
(load "msvc_functions")
(load "setup_cmake")
;; (load "setup_cmake-ide")
(load "setup_stuff_for_cc_mode")
(load "setup_dired")
(load "setup_qt")
(load "setup_python")
(load "setup_other_global_settings")
(load "setup_global_keys")
(load "setup_moon")
(load "setup_server")
(load "sql_setup")
system-type
(when (eq system-type 'darwin)
  (load "setup_macos"))


(setq software_version_string "")
;; (set-face-attribute 'default nil :height 90)
(transient-mark-mode 0)

