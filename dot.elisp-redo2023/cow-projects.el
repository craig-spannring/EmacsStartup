
(use-package projectile
             :ensure t)

 
(defun cow-switch-project (proj-file)
  "Herd the cow into a different project.
"
  (interactive (list
		(expand-file-name
		 (completing-read
		  ;; PROMPT
		  "Project File: "  
		  ;; COLLECTION
		  #'(lambda (name predicate how)
		      ;; For each one of the project types registered by
		      ;; cow-register-project-type, call the collection
		      ;; function
                      (message "In collection lambda.  how=%s" how)
                      (let* ((outcomes (mapcar #'(lambda (x)
						   (let* ((completion (car x))
							  (predicate  (cadr x)))
                                                     (message "looking at %s" x)
                                                     (funcall completion
							      name
							      predicate
							      how)))
                                               _cow-proj-handler-map)))
                        (message "outcomes is %s" outcomes)
		        "now"))
		  ;; PREDICATE
		  #'(lambda (x) (message "Note- this shouldn't get called. (filtering %s)" x))
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

(defun cow-register-project-type (completion predicate setup)
  "Register project type with the cow. 

PROJ is a dotted pair. The first item in the dotted is a
collection function.  This collection function takes 3 parmeters:
   -name        Current (partial) filename to expand.
   -predicate   A predicate function to filter out files
                that shouldn't be considered. 
   -how         See 'Programmed Completion' in elisp manual
                  nil   
                    The collection function should return 
                      - nil if no matches
                      - t if unique and exact match
                      - longest common prefix 
                  t
                    The collection function should return 
                      - list of all possible completions
                  lambda
                    The collection function should return 
                      - t if the specified string is an exact match 
                          for some completion alternative. 
                      - nil otherwise
                  (boundaries . suffix)
                    See completion-boundaries function in ELisp manual.
                  metadata
                  any other value for how
                    The collection function should return nil

The second item in each dotted pair is a function that will setup
for the given project."
  (let* ((proj (list completion predicate setup)))
    (if (not (member proj _cow-proj-handler-map))
	(progn
	  (message "trying to add %s" proj)
	  (setq _cow-proj-handler-map (cons proj _cow-proj-handler-map)))
      (message "not adding %s" proj))))

(defvar _cow-proj-handler-map nil
"Lookup table of project handlers.

This lookup table is a list of dotted pairs.  See
cow-register-project-type for information about the format of the
dotted pair.")


(provide 'cow-projects)
