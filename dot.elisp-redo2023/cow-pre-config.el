;; -*- lexical-binding: t; -*-

;;; Settings that should be made prior to loading our packages.
;;; 
;;;  This file
;;;   1) Sets up the custom-file to be outside of the version
;;;      controlled area.  That way if users customize any of the Emacs
;;;      behaviors it won't change any version controlled files. 
;;; 



(setq custom-file
      (let* ((tmp (expand-file-name
                   (format "%s/custom-%d-%s.el"
                           user-emacs-directory
                           emacs-major-version
                           cow-setup-era))))
        (if (file-exists-p tmp) (load tmp))
        tmp))

(defgroup cow-emacs-conf nil "Settings for Craig's Own Way")
