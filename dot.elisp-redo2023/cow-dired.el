;; Bring in dired and setup some global keybinds 
(require 'dired-x)

(use-package ztree :ensure t)

(provide 'cow-dired)


(let* ((windows-nt (string= system-type "windows-nt"))
       (predicates (concat  ;; List of files and directories to exclude from the find
                    (if t          " \\( ! -regex '.*/Debug[a-zA-Z0-9]*Board/.*' \\) " "")

                    (if windows-nt " \\( ! -regex '.*/Output/Debug/.*'   \\) "         "")     
                    (if windows-nt " \\( ! -regex '.*/Output/Release/.*' \\) "         "")
                                   
                    (if t          " \\( ! -regex '.*/.svn/.*' \\) "                   "")
                    (if t          " \\( ! -name '*~' \\) "                            "")
                    (if t          " \\( ! -name 'BROWSE' \\) "                        "")
                                   
                    (if t          " \\( ! -name '.QTmakefile*' \\) "                  "")
                    (if t          " \\( ! -name 'moc_*\\.cpp' \\) "                   "")
                    (if t          " \\( ! -regex '.*/\\.obj.*' \\) "                  "")
                    (if t          " \\( ! -regex '.*/\\.obj.*' \\) "                  "")))
       (pattern (cond  	    ;; Way to run find on current computer for grep-find and find-dired

                 (windows-nt
                  (format "gfind . %s -type f -print0 | xargs -0 -e grep -nIH -e  " predicates))
                 (t
		  (format "find . %s -type f -exec grep --color -nIH -e   {} +"     predicates))))
       (index (cond         ;; Index will help us position cursor in mini-buffer for grep-find 
                 (windows-nt   (length pattern))
                 (t            (- (length pattern) 5)))))

  ;; Setup to run find-dired
  (cond 
   (windows-nt
    (setq find-program            "gfind")
    (setq find-dired-find-program "gfind")
    (setq grep-find-command       (cons pattern index)))
   (t
    (cond 
     ((fboundp 'grep-apply-setting)
      (grep-apply-setting 'grep-find-command (cons pattern index)))
     (t
      (setq grep-find-command (cons pattern index)))))))
