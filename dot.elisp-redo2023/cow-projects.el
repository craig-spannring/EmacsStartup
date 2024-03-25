
(require 'cl-lib)
(use-package projectile
             :ensure t)

 
(defun cow-load-project (proj-file)
  "Move the cow into a different project.
"
  (interactive (list
		(expand-file-name
		 (read-file-name "Project File: " ;; PROMPT
				 nil              ;; DIR
				 nil              ;; DEFAULT-FILENAME
				 t                ;; MUSTMATCH
				 nil              ;; INITIAL
				 #'(lambda (f)    ;; PREDICATE
				     (or (mapcar
					  #'(lambda (h)
					      (funcall (car h) f))
					  _cow-proj-handler)))))))
 
  (let* ((setup (cdr (car (cl-remove-if-not
		      #'(lambda (x) (car x))
		      (mapcar #'(lambda (proj-bundle)
				  (let*
				      ((setup      (cadr proj-bundle))
				       (predicate  (car  proj-bundle)))
				    (cons (funcall predicate proj-file) setup)))
			      _cow-proj-handler))))))
    (message "proj-file is %s" proj-file)
    (message "setup is %s" setup)
    (setq _cow-project-info (funcall setup proj-file))))

(defun cow-compile-project ()
  (interactive)
  (when (null _cow-project-info) (error "Must load a project first"))
  (let* ((compile-func (cdr (assoc 'compile-func _cow-project-info))))
    (when (null compile-func)
      (error "Internal error: Project type doesn't provide 'compile-func"))
    (message "compile function is %s" compile-func)
    (funcall compile-func)))
    

(defun cow-register-project-type (predicate setup)
  "Register project type with the cow. 

PROJ is a list of three items. The first item in the dotted is a
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
for the given project.

The third item is a setup function.  The function must take a
single argument, the filename of the project.  The function
should return a plist with the keys
  compile
  proj-file
"
  (let* ((proj (list predicate setup)))
    (if (not (member proj _cow-proj-handler))
	(progn
	  (message "trying to add %s" proj)
	  (setq _cow-proj-handler (cons proj _cow-proj-handler)))
      (message "not adding %s" proj))))

(defun _cowguts-files-and-dirs-by-prefix (dir prefix)
  "Return a list of file and directories located in DIR starting with PREFIX"
  (if (not (file-directory-p dir)) (error "DIR is not a directory"))
  
  (directory-files dir t (concat "^" (regexp-quote prefix))))

(defun cowguts-dirs-by-prefix (dir prefix)
    "Return a list of all directories in DIR that start with PREFIX

All directories returned will be fully qualified have a trailing
slash."
    (mapcar #'file-name-as-directory
	    (cl-remove-if-not #'file-directory-p
			      (_cowguts-files-and-dirs-by-prefix dir prefix))))
(defun cowguts-files-by-prefix (dir prefix)
    "Return a list of all directories in DIR that start with PREFIX

All directories returned will be fully qualified have a trailing
slash."
    (cl-remove-if #'file-directory-p
		  (_cowguts-files-and-dirs-by-prefix dir prefix)))

(defun cowguts-handle-// (f)
  "Handle a // in a pathname.

If f does not contain any repeated slashes then f is returned
unaltered.  Otherwise this returns the portion of the string that
comes after the last repeated slashes."

  (if (not (cl-search "//" f)) f
    (let* ((remaining (car (last  (split-string f "//+")))))
      (cond
       ((or (string-prefix-p "~/"  remaining)
	    (string-prefix-p "./"  remaining)
	    (string-prefix-p "../" remaining)
	    (equal "."  remaining)
	    (equal ".." remaining)
	    (equal "~"  remaining))
	remaining)
       (t (concat "/" remaining))))))

	                  
(defvar _cow-proj-handler nil
"Table of project handlers.

This lookup table contains a list of dotted pairs.  The first
element is a predicate function.  The second element is a setup
function.  See cow-register-project-type for more information.")

(defvar _cow-project-info nil
  "Association list with information about the current project
including project file name and a function to compile the
project. ")
  


(provide 'cow-projects)
