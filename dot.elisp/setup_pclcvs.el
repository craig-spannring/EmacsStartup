;;;
;(autoload 'cvs-update "pcl-cvs"
;	  "Run a 'cvs update' in the current working directory. Feed the
;output to a *cvs* buffer and run cvs-mode on it.
;If optional prefix argument LOCAL is non-nil, 'cvs update -l' is run."
;	  t)

;(autoload 'cvs-update-other-window "pcl-cvs"
;	  "Run a 'cvs update' in the current working directory. Feed the
;output to a *cvs* buffer, display it in the other window, and run
;cvs-mode on it.


(when (not (functionp 'cvs-examine))
  (load "pcl-cvs-startup"))

(setq cvs-update-optional-flags '("-d -P"))


(cond 
 ((string= system-type "windows-nt")
  (setq cvs-program "c:/bin/cvs.exe")
  (setq cvs-diff-program "C:/cygwin/bin/diff.exe")
  (setq cvs-shell "c:/cygwin/bin/bash.exe")
))


(defun cvs-log-edit-insert-pr-summary (callback &optional setup listfun buffer &rest ignore)
  (cond ((null setup)
         (save-excursion
           (when buffer (set-buffer buffer))
           (cvs-insert-pr-summary))))
  (log-edit callback setup listfun buffer))

(defun cvs-insert-pr-summary ()
  "Ask the user for a PR number and insert the PR's synopsis into the current buffer"
  (let
   ((original-point (point))
    pr-query-command
    pr-summary
    old-prs
    (pr (read-string "PR's: ")))

   (cond 
    ((string-equal pr "")
     (setq pr-summary "\n"))
    ((string-equal pr "0")
     (setq pr-summary 
           "Related PR: unknown/0  Miscellaneous change.\n"))
    (t
     (setq pr-query-command (format "%s %s %s" 
                                    cvs-gnats-query-prog 
                                    cvs-gnats-query-prog-args
                                    pr))
     (message "Executing %s" pr-query-command)
     (setq pr-summary (shell-command-to-string pr-query-command))))

   (message "PRs is %s and the string is %s" pr pr-summary)
   (goto-char (point-min))
   (while (and
           (> (length (buffer-string)) (length cvs-gnats-log-prefix))
           (equal cvs-gnats-log-prefix 
                 (buffer-substring (point-min) 
                                   (+ (point-min) 
                                      (length cvs-gnats-log-prefix)))))
                                                           
     (goto-line 2)
     (beginning-of-line)
     (delete-region (point-min) (point))
     )
     
   (insert (format "%s" pr-summary))
   (goto-char (point-max))
   )
)

;; (defvar cvs-gnats-query-prog "/usr/local/bin/query-pr-for-cvs")
;;(defvar cvs-gnats-query-prog "ssh -n mmled.aedinc.net /usr/local/bin/query-pr-for-cvs")
(defvar cvs-gnats-query-prog 
  (cond 
   ((or (string= system-name "MARS")
        (string= system-name "HYDRA"))
        "c:/bin/query-pr-for-cvs.py")
   ((string= system-type "windows-nt") "sh c:/bin/query-pr-for-cvs.sh")
   (t "query-pr-for-cvs")))

(defvar cvs-gnats-query-prog-args "")
(defvar cvs-gnats-log-prefix "Related PR: ")


                    
