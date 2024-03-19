
(use-package projectile
             :ensure t)

(defvar _cow-proj-handler-map nil
"Lookup table of project handlers

This lookup table is a list of dotted pairs.  

The first item in each dotted is a predicate function.  This
predicate function takes 3 parmeters:
   -name        Current (partial) filename to expand.
   -predicate   
   -how

The second item in each dotted pair is a function that will setup 
for the given project.")

(defun _cow-register-project-type (proj)
  (if (not (member proj _cow-proj-handler-map))
      (progn
	(message "trying to add %s" proj)
	(setq _cow-proj-handler-map (cons proj _cow-proj-handler-map)))
    (message "not adding %s" proj))
  )

(defun mypred (name predicate how) t)
(defun mysetup () nil)
 
(_cow-register-project-type '(mypred mysetup))
(_cow-register-project-type '(mypred mysetup))

(defun cow-switch-project (proj-file)
  (interactive (list (expand-file-name
		      (completing-read
		       ;; PROMPT
		       "Project File: "  
		       ;; COLLECTION
		       #'(lambda (name predicate how)
			   (message "The predicate returns %s" (funcall predicate "hello"))
			   (message "partial name is %s" name)
			   "now")
		       ;; PREDICATE
		       #'(lambda (x) (message "is filtering %s" x))
		       ;; REQUIRE-MATCH
		       t                 
		       ;; INITIAL-INPUT
		       nil              
		       ;; HIST
		       nil 
		       ;; DEF
		       nil
		       ;; INHERIT-INPUT-METHOD
		       nil))))
  (message "inside cow-switch-project")
  (message "proj-file is %s" proj-file))

;; (defalias 'switch-project cow-switch-project)

(provide 'cow-projects)
