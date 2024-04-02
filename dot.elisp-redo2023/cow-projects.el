;;;
;;; The COW method to handle projects.
;;;

(require 'cl-lib)
(use-package projectile
             :ensure t)


(defun _cow-find-file-no-proj () (interactive)
       "Let user know if no project is loaded."
       (message "No project loaded or find-file is not implemented yet."))
(global-set-key [f9 ?f] '_cow-find-file-no-proj)


(defun cow-load-project (proj-file)
  "Load a different project into COW"
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

(defun cow-current-project-file()
  "Print the current project in the mini-buffer"
  (interactive)
  (let ((proj-file (if _cow-project-info
                       (cdr (assoc 'proj-file _cow-project-info))
                     "None")))
    (message "Current project: %s" proj-file)
    proj-file))
  
(defun cow-compile-project ()
  "Run compile commands for a loaded project. "
  (interactive)
  (when (null _cow-project-info) (error "Error: must load a project first."))
  (let* ((compile-func (cdr (assoc 'compile-func _cow-project-info))))
    (when (null compile-func)
      (error "Internal error: Project type doesn't provide 'compile-func"))
    (message "compile function is %s" compile-func)
    (funcall compile-func)))
    

(defun cowguts-register-project-type (predicate setup)
  "Register project type with the COW. 

PREDICATE is a function to see if a file name is a particular
type of project file.  The fuction must take a pathname and
return non-nil if that file is a project file for this particular
project type.

SETUP is the function that will configure how we compile the
project.  The function must take a single argument, the filename
of the project.  The function should return a plist with the keys
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


(defvar _cow-proj-handler nil
"Table of project handlers.

This lookup table contains a list of dotted pairs.  The first
element is a predicate function.  The second element is a setup
function.  See cowguts-register-project-type for more information.")

(defvar _cow-project-info nil
  "Association list with information about the current project.
The information includes at least:
  - project file name 
  - function to compile the project. ")


(require 'cow-cpp-setup)

(provide 'cow-projects)
