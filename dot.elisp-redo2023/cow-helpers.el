;;;
;;; Helper functions
;;;

(defun set-tab-width (w)
   (interactive "nWidth: ")
   (setq tab-width w))

(defun cowguts-all-dot-c-cpp-h-and-hpp (path)
  "Return all files with a standard C++ extension

Find all files ending with .c, .cpp, .h, or .hpp in the directory
tree at PATH.  Return them a hash table where the key is the
basename and the value is the full path."

  (let* ((file-lookup-table (make-hash-table :test 'equal)))
    (mapc #'(lambda (full-path)
              (puthash (file-name-nondirectory full-path)
                       full-path
                       file-lookup-table))
          (directory-files-recursively path "\\.\\(cpp\\|c\\|h\\|hpp\\)$"))
    file-lookup-table))
  
(provide 'cow-helpers)
