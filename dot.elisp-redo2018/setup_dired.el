
(require 'dired-x)


(let* ((predicates (concat 
                    (if (string= system-type "windows-nt")        " \\( ! -regex '.*/Output/Debug/.*'   \\) " "")
                    (if (string= system-type "windows-nt")        " \\( ! -regex '.*/Output/Release/.*' \\) " "")
                    (if t                                         " \\( ! -regex '.*/.svn/.*' \\) "           "")
                    (if t                                         " \\( ! -name '*~' \\) "                    "")
                    (if t                                         " \\( ! -name 'BROWSE' \\) "                "")
                    (if (not (string= system-type "windows-nt"))  " \\( ! -name '.QTmakefile*' \\) "          "")
                    (if (not (string= system-type "windows-nt"))  " \\( ! -name 'moc_*\\.cpp' \\) "             "")
                    (if (not (string= system-type "windows-nt"))  " \\( ! -regex '.*/\\.obj.*' \\) "            "")
                    (if (not (string= system-type "windows-nt"))  " \\( ! -regex '.*/\\.obj.*' \\) "            "")))
       (pattern (cond
                 ((string= system-type "windows-nt")
                  (format "gfind . %s -type f -print0 | xargs -0 -e grep -nIH -e  " predicates))
                 ;((string= system-type "darwin")
                 ; (format "find . %s -type f -print0 | xargs -0 grep -nIH -e " predicates))
                 (t
		  (format "find . %s -type f -exec grep --color -nIH -e   {} +" predicates))))
       (index (cond
                 ((string= system-type "windows-nt")   (length pattern))
                 ;((string= system-type "darwin")       (length pattern))
                 (t                                    (- (length pattern) 5)))))
  
  (cond 
   ((string= system-type "windows-nt")
    (setq find-program "gfind")
    (setq find-dired-find-program "gfind")
    (setq grep-find-command (cons pattern index)))
   (t
    (cond 
     ((fboundp 'grep-apply-setting)
      (grep-apply-setting 'grep-find-command (cons pattern index)))
     (t (setq grep-find-command (cons pattern index)))))))
