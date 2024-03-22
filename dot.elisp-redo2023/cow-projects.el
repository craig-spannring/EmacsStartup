
(require 'cl-lib)
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
                      (message "In collection function.  how=%s" how)
                      (let*
			  ((outcomes (mapcar
				      #'(lambda (proj-bundle)
					  (let*
					      ((completion (car  proj-bundle))
					       (predicate  (cadr proj-bundle)))
					    (funcall completion
						     name
						     predicate
						     how)))
                                      _cow-proj-handler)))
                        (message "outcomes is %s" outcomes)
			;; TODO figure out how to colapse all the outcomes into one
			(cond
			 ((member t outcomes) t)
			 (t nil))))
		  ;; PREDICATE
		  #'(lambda (x) (message "Note- Shouldn't get here. |%s|" x))
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
  (let* ((setup (cdr (car (cl-remove-if-not
		      #'(lambda (x) (car x))
		      (mapcar #'(lambda (proj-bundle)
				  (let*
				      ((setup      (caddr proj-bundle))
				       (predicate  (cadr  proj-bundle)))
				    (cons (funcall predicate proj-file) setup)))
			      _cow-proj-handler))))))
    (message "proj-file is %s" proj-file)
    (message "setup is %s" setup)
    (funcall setup proj-file)))


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
    (if (not (member proj _cow-proj-handler))
	(progn
	  (message "trying to add %s" proj)
	  (setq _cow-proj-handler (cons proj _cow-proj-handler)))
      (message "not adding %s" proj))))

(defvar _cow-proj-handler nil
"Lookup table of project handlers.

This lookup table is a list of dotted pairs.  See
cow-register-project-type for information about the format of the
dotted pair.")


(provide 'cow-projects)
