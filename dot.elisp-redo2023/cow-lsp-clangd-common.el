


(defun cow-clangd-select-a-compile-commands-json (path)
  ;; TODO think we need to call lsp-workspace-folders-add
  (lsp-workspace-folders-add (file-name-directory  path))

  (global-set-key
   [f9 ?f] 
   #'(lambda (file-name)
       (interactive)
       (_cow-lsp-clangd-find-file-in-project (file-name
                                             file-name-directory  path))))

  (message "%s\nTODO- flesh out the LSP cpp support" path))

(defun _cow-lsp-clangd-find-file-in-project (file-name top-dir)
  (interactive)
  (message "Project: %s\nFile:    %s\nTODO- need to implement this"))


(provide 'cow-lsp-clangd-common)
