
(use-package projectile
             :ensure t)

;; (defvar cow-proj-handler-map nil)
;;   nill
;;   """Lookup table of project handlers
;; 
;; This lookup table is a list of dotted pairs.  
;; 
;; The first item in each dotted is a predicate function.  This
;; predicate function takes a pathname of a file that represents a
;; 3rd-party project file (e.g. ~/foo.vcproj.
;; 
;; The second item in each dotted pair is a function that will setup 
;; for the fiven project.
;; """)

(defun cow-switch-project (proj-file)
  (interactive)
  (message "proj-file is %s" proj-file))
  
;; (defalias switch-project cow-switch-project)

(provide 'cow-projects)
