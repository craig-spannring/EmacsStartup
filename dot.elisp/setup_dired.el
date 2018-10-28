(cond
 ((string-match "XEmacs" emacs-version)
  nil)
 (t
  ;;
  ;; Set up the dired functions.
  ;;
  (autoload 'find-dired "find-dired" nil t)
  (autoload 'find-name-dired "find-dired" nil t)
  (autoload 'find-grep-dired "find-dired" nil t)
  
  
  ;;
  ;;
  ;; bind some keys for the dired find functions
  ;;
  (global-set-key "\C-cf" 'find-dired)
  (global-set-key "\C-cn" 'find-name-dired)
  (global-set-key "\C-cl" 'find-grep-dired)))

;;
;; Setup the default pattern for the grep-find command
;;
(let* ((predicates (concat 
                    (if (string= system-type "windows-nt")        " \\( ! -regex '.*/Output/Debug/.*'   \\) " "")
                    (if (string= system-type "windows-nt")        " \\( ! -regex '.*/Output/Release/.*' \\) " "")
                    (if t                                         " \\( ! -regex '.*/.svn/.*' \\) "           "")
                    (if t                                         " \\( ! -name '*~' \\) "                    "")
                    (if t                                         " \\( ! -name 'BROWSE' \\) "                "")
                    (if (not (string= system-type "windows-nt"))  " \\( ! -name '.QTmakefile*' \\) "          "")
                    (if (not (string= system-type "windows-nt"))  " \\( ! -name 'moc_*.cpp' \\) "             "")
                    (if (not (string= system-type "windows-nt"))  " \\( ! -regex '.*/.obj.*' \\) "            "")
                    (if (not (string= system-type "windows-nt"))  " \\( ! -regex '.*/.obj.*' \\) "            "")))
       (pattern (cond
                 ((string= system-type "windows-nt")
                  (format "gfind . %s -type f -print0 | xargs -0 -e grep -nIH -e  " predicates))
                 ((string= system-type "darwin")
                  (format "find . %s -type f -print0 | xargs -0 grep -nIH -e " predicates))
                 (t                                                                                                                               
                  (format "find . %s -type f -print0 | xargs -0 -e grep -nIH -e  " predicates)))))
  (cond 
   ((string= system-type "windows-nt")
    (setq find-program "gfind")
    (setq find-dired-find-program "gfind")
    (setq grep-find-command (cons pattern (length pattern))))
   (t
    (cond 
     ((fboundp 'grep-apply-setting) 
      (grep-apply-setting 'grep-find-command (cons pattern (length pattern))))
     (t (setq grep-find-command (cons pattern (length pattern))))))))


(add-hook 'dired-load-hook
          (lambda ()
            (message "dired-x")
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1)
            ))
