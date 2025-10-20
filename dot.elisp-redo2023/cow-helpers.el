;;;
;;; Helper functions
;;;

(defun set-tab-width (w)
   (interactive "nWidth: ")
   (setq tab-width w))

(defun cowguts-all-dot-c-cpp-h-and-hpp (path)
  "Return all files with a standard C++ extension.

Find all files ending with .c, .cpp, .h, or .hpp in the directory
tree at PATH.  Return them a hash table where the key is the
basename and the value is the full path of all files with that
basename."

  (let* ((file-lookup-table (make-hash-table :test 'equal)))
    (mapc
     #'(lambda (full-path)
         (let* ((basename  (file-name-nondirectory full-path))
                (prev      (gethash basename file-lookup-table nil))
                (new-value (cons full-path prev)))
           (puthash basename
                    new-value
                    file-lookup-table)))
     (directory-files-recursively path "\\.\\(cpp\\|c\\|h\\|hpp\\)$"))
    file-lookup-table))


(defun _cow-ensure-sep (x)
  (if (string-suffix-p "/" x) x (concat x "/")))


(defun cowguts-join-paths (top &rest parts)
  "Join path components

Join all components into a path, separating each with a / if
needed.  If any component starts with a / the previous
components will be discarded.  if the last component is the
empty string, this will ensure the result ends with a slash.
"
  (cond
   ;; parts is nil                        
   ((not parts) 
    top)
   ;; first item in parts starts with a "/"
   ((string-prefix-p "/" (car parts))
    (if (not (cdr parts))
        top
      (apply #'cowguts-join-paths
             (car parts)
             (cdr parts))))
   ;; subdir has one item which is ""
   ((equal '("") parts)
    (_cow-ensure-sep top))
   ;; (cdr parts) is nil
   ((not (cdr parts))
    (concat (_cow-ensure-sep top) (car parts)))
   ;; none of the above are true, default case. 
   (t
    (apply #'cowguts-join-paths
           (concat (_cow-ensure-sep top) (car parts))
           (cdr parts)))))

(defun cowguts-salt-licks-dir()
  "Scratch directory for COW"
  (expand-file-name "~/tmp/cow-salt-licks"))

  
(provide 'cow-helpers)
