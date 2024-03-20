
(use-package projectile
             :ensure t)

 
(defun cow-switch-project (proj-file)
  "Herd the cow into a different project.
"
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

(defun cow-register-project-type (proj)
  "Register project type with the cow. 

PROJ is a dotted pair. The first item in the dotted is a
predicate function.  This predicate function takes 3 parmeters:
   -name        Current (partial) filename to expand.
   -predicate   A predicate function to filter out files
                that shouldn't be considered. 
   -how         See 'Programmed Completion' in elisp manual
                  nil   
                  t
                  lambda
                  (boundaries . suffix)
                  metadata
                  Any other value

The second item in each dotted pair is a function that will setup
for the given project."
  
  (if (not (member proj _cow-proj-handler-map))
      (progn
	(message "trying to add %s" proj)
	(setq _cow-proj-handler-map (cons proj _cow-proj-handler-map)))
    (message "not adding %s" proj))
  )

(defvar _cow-proj-handler-map nil
"Lookup table of project handlers.

This lookup table is a list of dotted pairs.  See
cow-register-project-type for information about the format of the
dotted pair.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove this block                              ;;
(defun myexpander  (name predicate how) t)        ;;
(defun mysetup     (name)               nil)	  ;;
(cow-register-project-type '(myexpander mysetup)) ;;
(cow-register-project-type '(myexpander mysetup)) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cow-projects)
