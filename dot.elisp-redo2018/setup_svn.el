;;

(defun determine_svn_major_minor ()
  (let ((tmp 
         (condition-case nil
             (let* ((version-list (split-string (shell-command-to-string "svn --version -q") "[.]"))
                    (major (string-to-number (car version-list)))
                    (minor (string-to-number (cadr version-list))))
               (list major minor))
           (error nil))))
    (cond (tmp tmp)
          (t   (list 0 0)))))
 
(defcustom subversion-preferred-package 'dsvn
  "*Determine which Emacs package to use for Subversion. 
Note- Changes won't take effect until you restart emacs.

"
  :type '(choice (const psvn)
                 (const dsvn))
  :group 'cts-setup-subversion)



(autoload 'svn-status "setup_svn2" "Run `svn status'." t)

;;   (error nil))



(defcustom svn_bz-synopsis-prog "bz_synopsis"
  "program that will return the synopsis for one or more bugs"
  :group 'cts-setup-subversion)
(defcustom svn_bz-synopsis-prog-args 
  "--user=bugs_reader --dbname=bugs --host=spibugtrk1.spray.com --svn"
  "command line arguments to pass to `svn_bz-synopsis-prog'"
  :group 'cts-setup-subversion)


(defvar svn_bz-previous-hours nil)
(defvar svn_bz-previous-synopsis nil)
(setq svn_bz-bug-id-history '("0"))

(defun teejet-goto-end-of-summary-line ()
  ;; 
  ;; Try to find the synopsis (named "Summary: ") line and 
  ;; put the cursor at the end of that line
  ;; 
  (goto-char (point-min))
  (message "Searching for summary"); 
  (search-forward-regexp "^Summary: " (point-max) t)
      (message "point is now %s" (point)))

(defun teejet-remove-log-edit-author ()
  "Remove the Author: line that log edit seems to think we want."

  (message "running teejet-remove-log-edit-author")
  (save-excursion 
    (goto-char (point-min))
    ;; (message "need to search")
    (while (search-forward "Author: " (point-max) 1)
        ;; (message "search. point is %d" (point))
        (replace-match "" nil t))))


(defun svn_bz-insert-bugzilla-synopsis ()
  "Lookup bug(s) synopsis and insert at first line in current buffer

  This function is suitable for use as an advise function for 
  the svn-status-commit function. 
  "
  (interactive)
  (message "running svn_bz-insert-bugzilla-synopsis")
  (save-excursion 
    ;;
    ;; First remove the "Related PR:" lines that we added last time
    ;; around.
    ;;     
    (goto-char (point-min))
    (when (and 
           (not (null svn_bz-previous-synopsis))
           (> (length svn_bz-previous-synopsis) 0))
      ;; (message "need to search")
      (while (search-forward svn_bz-previous-synopsis (point-max) 1)
        ;; (message "search. point is %d" (point))
        (replace-match "" nil t)))
    ;; 
    ;; And remove the previous hours line
    ;; 
    (goto-char (point-min))
    (when (and 
           (not (null svn_bz-previous-hours))
           (> (length svn_bz-previous-hours) 0))
      (let* ((previous-hours (format "Hours: %s" svn_bz-previous-hours)))
        ;; (message "need to search")
        (while (search-forward previous-hours (point-max) 1)
          ;; (message "search. point is %d" (point))
          (replace-match "" nil t))))
      
    
    ;;
    ;; Ask user what bug ids are associated with this
    ;; commit.
    ;; 
    (let ((ids   (read-string "Bug ID's: " nil 'svn_bz-bug-id-history))
          (hours (read-string "Hours Worked: " nil nil "0"))
          synopsis)
      
      ;;
      ;; Retrive the bug descriptions from bugzilla
      ;;
      (cond 
       ((string-equal ids "")
        (setq synopsis ""))
       ((string-equal ids "0")
        (setq synopsis 
              "Related PR: unknown/0  Miscellaneous change.\n"))
       (t
        (setq pr-query-command (format "%s %s %s" 
                                       svn_bz-synopsis-prog
                                       svn_bz-synopsis-prog-args
                                       ids))
        (message "Executing %s" pr-query-command)
        (setq synopsis (shell-command-to-string pr-query-command))))
      
      (when (not (string-equal hours "0"))
        ;; 
        ;; insert the number of hours worked. 
        ;; 
        (goto-char (point-min))
        (insert (format "Hours: %s\n" hours)))
      
      ;; 
      ;; Now insert the descriptions at the first line of the buffer
      ;; 
      (goto-char (point-min))
      (insert synopsis)

      (let* ((cursor-final-pos (point))
	     (summary "Subject: \n"))
	(when (>= emacs-major-version 25)
	  (insert "Subject: \n")
	  (setq cursor-final-pos (- (point) (- cursor-final-pos 1))))
	(message "cursor-final-pos is %d" cursor-final-pos)
	(goto-char cursor-final-pos))

      ;; 
      ;; Next time around we will need to remove the synopsis lines
      ;; and hours.  Squirrel them away for next time around.
      ;; 
      (setq svn_bz-previous-hours    hours)
      (setq svn_bz-previous-synopsis synopsis))))


(defadvice svn-status-commit (after get-bugzilla-info activate)
  (svn_bz-insert-bugzilla-synopsis))
