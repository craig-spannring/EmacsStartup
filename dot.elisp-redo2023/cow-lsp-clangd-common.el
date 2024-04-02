;;; -*- lexical-binding: t -*-



(defun cow-clangd-select-a-compile-commands-json (path)
  "Load a compile_commands.json. 
Load the json from PATH."

  (lsp-workspace-folders-add (file-name-directory  path))
  
  (global-set-key
   [f9 ?f] 
   (lambda (file-name)
     ;; (interactive "f")
     (interactive
      (list
       (completing-read "Find C++ File: "
                        (cond
                         ((not (_msvc-have-current-project))
                          (error "Current project is not set.  Use M-x msvc-load-project")
                          nil)
                         (t (_msvc-filelookup-files-alist)))
                        nil     ; predicate
                        t       ; require-match
                        nil     ; init
                        nil     ; hist
                        nil)))  ; def
     
     (message "In lambda (%s) path=%s" file-name path)
     (_cow-lsp-clangd-find-file-in-project
      file-name
      (file-name-directory path))))

  (message "%s" path))

(defun _cow-lsp-clangd-find-file-in-project (file-name top-dir)
    (message "Project: %s\nFile:    %s\nTODO- need to implement this" top-dir file-name))
   

(provide 'cow-lsp-clangd-common)
