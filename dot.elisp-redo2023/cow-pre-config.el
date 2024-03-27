;;; Settings that should be made prior to loading our packages.

(setq custom-file
      (let* ((tmp (expand-file-name
                   (format "%s/custom-%d-%s.el"
                           user-emacs-directory
                           emacs-major-version
                           cow-setup-era))))
        (if (file-exists-p tmp) (load tmp))
        tmp))

(defgroup cow-emacs-conf nil "Settings for Craig's Own Way")
