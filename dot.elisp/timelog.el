;;; timelog.el ---  log and summarize your working times in useful ways

;; Copyright (C) 2000, 2001 Thomas Gehrlein <thomas.gehrlein@t-online.de>

;; Author: Thomas Gehrlein <thomas.gehrlein@t-online.de>
;; Maintainer: Thomas Gehrlein <thomas.gehrlein@t-online.de>
;; Created: 23 Feb 2001
;; Version: 1.1
;; Keywords: calendar data time

;; This file is not part of GNU Emacs.

;; timelog.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:

;; This file helps you to keep track of what you do.  It writes information
;; into a file.  Each line in this file consists of a code letter (i or o - "i"
;; means "in", i.e. starting an activity, "o" means "out", i.e. finishing an
;; activity), a date and time stamp, and information supplied by the user.
;; Example (w/o the ";;", of course):

;; i 2001/02/21 11:07 timelog
;; o 2001/02/21 11:53 code writing
;; i 2001/02/21 13:46 timelog
;; o 2001/02/21 14:12 debugging
;; i 2001/02/21 14:24 timelog
;; o 2001/02/21 14:33 debugging

;; The data in this file can then be summarized in amazing ways.

;; differences between timeclock.el and timelog.el:
;; You don't give a reason for logging out.  Instead you specify what you've
;; done since you logged in.  (Reason is, I want to record
;; what I did, not why I stopped doing it.)
;; timelog.el doesn't keep track of how much time there is left to work for
;; today (or tomorrow, or the day after).

;; timeclock.el uses more code letters, timelog.el uses only i and o.
;; timeclock.el uses hh:mm:ss, timelog.el uses hh:mm
;; Maybe I should use the same time format?  Maybe I should use timeclock.el to
;; write the log-file and keep only that part of my code that I need to read
;; it?

;; To enter a SPC in the completion-minibuffer use C-q SPC or (slightly less
;; annoying) M-SPC (bound to just-one-space).  (Thanks to
;; <Kai.Grossjohann@CS.Uni-Dortmund.DE>)


;;; RESTRICTIONS/PROBLEMS
;; The time format in the timelog file cannot be changed.  Project and
;; subproject entries cannot include newlines.

;; Blank lines in the timelog file confuse timelog.el.  So better avoid them
;; when you edit the file manually.

;; timelog.el supports only one timelog file.  Support for multiple files is
;; not planned.  (Workaround: set the variable timelog-file to a new value and
;; then timelog-reread-logfile).  Note that it is possible to have several
;; projects in your timelog-file

;; timelog-summarize-project doesn't work properly if you log out and forget to
;; enter a subproject.  It cannot handle "" as subproject string.  If your
;; timelog-file has missing projects or subprojects, use
;; timelog-add-omitted-projects first.


;;; TODO (POSSIBLE IMPROVEMENTS/RANDOM THOUGHTS)

;; Make it comply with ell standard or sth.

;; timelog is sufficient for my needs.  I do not plan do add more functions in
;; the future.

;; Format 1000,00 -> 1.000,00
;; 1000.00 -> 1,000.00

;; timelog-make-subproject-list and timelog-read-subprojects could be one
;; function ("Make a list of all subprojects in TIMELOG-FILE.  If optional arg
;; PROJECT, make a list of subprojects of PROJECT only.")

;; Add defun for summarizing the work times of all days in ~/.timelog.  Useful
;; for finding out how much you really worked, if you work on more than one
;; projects.

;; Comments are welcome.  Especially appreciated: "Your code works, but this is
;; not the best/canonical way to do it ..." "Have you considered using
;; some-wonderful-function-that-is-absolutely-perfect-for-what-you-need."  "Your
;; code violates ancient elisp coding traditions, like ..."

 
;;; SETUP AND KEY-BINDINGS
;; Put timelog where your load-path finds it.
;; Key-bindings as suggested by John Wiegley in timeclock.el.  Add something
;; like the following lines to your .emacs.  (Remove the comments (;;) first.)
;; Type C-xC-e at the end of each line.  Test it.
;; (require 'timelog)
;; (define-key ctl-x-map "ti" 'timelog-in)
;; (define-key ctl-x-map "to" 'timelog-out)
;; (define-key ctl-x-map "tc" 'timelog-change)
;; (define-key ctl-x-map "ts" 'timelog-make-summary)
;; (define-key ctl-x-map "tr" 'timelog-reread-logfile)

;; This is what I have in my .emacs
;; (setq timelog-hourly-rate 50
;;       timelog-currency-string "DM"
;;       timelog-currency-separator-string ","
;;       timelog-summary-function 'timelog-my-summarize-project)


;;; Commentary:
;; 

;;; HISTORY
;; timelog 0.1
;; First draft.  No good.

;; timelog 0.2
;; Initial revision.  It works.  Tested by some people.

;; timelog 0.3
;; Minor bugfixes and code cleaning.  All functions read their args with completion.
;; (Except where it doesn't make sense).  The summary layout can be customized
;; in amazing ways.

;; timelog 0.4
;; Uses defcustom instead of defvars.  checkdoc'd.

;; timelog 1.0
;; Added timelog-make-summary.  timelog know does what I want it to do.

;; timelog 1.1
;; code cleaning.  Posted to gnu.emacs.sources

;;; Code:
;; User Variables:
(defgroup timelog nil
  "Logging and evaluating time spent on projects."
  :group 'data)

(defcustom timelog-file "~/.timelog"
  "*File to store timelog information."
  :type 'file
  :group 'timelog)

(defcustom timelog-summary-include-subprojects t
  "*Non-nil means:  Include time spent on subprojects in the summary."
  :type 'boolean
  :group 'timelog)

(defcustom timelog-summary-include-logfile t
  "*Non-nil means:  Include relevant lines from `timelog-file' in the summary."
  :type 'boolean
  :group 'timelog)

(defcustom timelog-summary-include-days t
  "*Non-nil means:  Include time spent on each day in the summary."
  :type 'boolean
  :group 'timelog)

(defcustom timelog-summary-include-rates t
  "*Non-nil means: Include your rates in the summary.

Adds for overall-time and for every subproject a string that indicates how much
you charge for this service.  The rate is computed using TIMELOG-HOURLY-RATE.
The currency is determined by TIMELOG-CURRENCY."
  :type 'boolean
  :group 'timelog)

(defcustom timelog-hourly-rate 100
  "*Number of currency units you charge for one hour of your time."
  :type 'integer
  :group 'timelog)

(defcustom timelog-currency-string "US-$"
  "*The currency you charge your rates in."
  :type 'string
  :group 'timelog)

(defcustom timelog-currency-separator-string "."
  "*String that separates big currency units from small currency units.

Like dollars and cents.  They use \".\".  We use \",\"."
  :type 'string
  :group 'timelog)

(defcustom timelog-summary-function 'timelog-summarize-project
  "*Function for summarizing a project.

If you don't like the default summary by `timelog-summarize-project' write your
own function.  See `timelog-my-summarize-project' for an example."
  :type 'function
  :group 'timelog)
 
(defcustom timelog-out-with-message t
  "*Non-nil means: When logging out, indicate how much time you worked."
  :type 'boolean
  :group 'timelog)

;;; Internal Variables:
(defvar timelog-version 1.1
  "This version of timelog.")

(defvar timelog-last-event nil
  "Last logging event: in or out.  \"i\" is \"in\" and \"o\" is \"out\".")

;; You don't always get the last project with (car timelog-project-list)
(defvar timelog-last-project nil
  "The project you specified when you last logged in.")

(defvar timelog-last-subproject nil
  "The subproject you specified when you last logged out.")

(defvar timelog-project-list nil
  "List of timelog projects, is read from the `timelog-file'.
Used for completion (and maybe other things as well).")

(defvar timelog-subproject-list nil
  "List of things that are parts of projects.  Used for completion and so.
There is only one list for all projects.")

(defvar timelog-time-string "%Y/%m/%d %H:%M"
  "String used by `format-time-string'.
Don't change it.  There's so much that depends on it.")

 
;;; File conversion
;; timeclock.el by John Wiegley <johnw@gnu.org> uses a _slightly_ different
;; format: it records the current time as hh:mm:ss (timelog-2.el uses hh:mm)

(defun timelog-remove-seconds ()
  "Remove the seconds from a timeclock file to convert it into a timelog file:
hh:mm:ss -> hh:mm"
  (interactive)
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))
    ;; search for seconds and remove them
    (while
        (re-search-forward
;;       "^[io] [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]"
         "^[io] [0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+"
                                        ;"i 2000/10/17 12:34" or
                                        ;"o 2000/10/17 12:34" at BOL
         nil t)
      (delete-char 3))
    (bury-buffer)))

(defun timelog-add-seconds ()
  "Add seconds to a timelog file to convert it into a timeclock file:
hh:mm -> hh:mm:ss"
  (interactive)
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))
    ;; search for where seconds should be and insert them
    (while
        (re-search-forward
;;       "^[io] [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9]"
         "^[io] [0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+"
                                        ;"i 2000/10/17 12:34" or
                                        ;"o 2000/10/17 12:34" at BOL
         nil t)
      (insert ":00"))

    (bury-buffer)))


(defun timelog-add-omitted-projects ()
  "Find lines with date and time but no project or subproject in `timelog-file'.

Enter some string.  `timelog-summarize-project' doesn't work properly with
missing project and subproject names."
  (interactive)
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))

    (while
        (re-search-forward
;;       "^[io] [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] \
;; [0-9][0-9]:[0-9][0-9]$" nil t)
         "^[io] [0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+$" nil t)
      (insert " nn"))

    (bury-buffer)))


;;; reading information from the timelog-file

(defun timelog-make-project-list ()
  "Make a list of projects from the TIMELOG-FILE.  Set `timelog-last-project'.

A project is everything you specify, when you log in."
  (interactive)
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))
    ;; make the list (search for anything that comes after a time string at BOL
    ;; and before EOL or EOF)
    (while
        (re-search-forward
;;       "^i [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9] "
         "^i [0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+ "
                                        ;"i 2000/10/17 12:34 " at BOL
         nil t)
      (add-to-list 'timelog-project-list
                   (buffer-substring (match-end 0) (search-forward-regexp
                                                    "$"))))
    (setq timelog-last-project (car timelog-project-list))
    (bury-buffer)))


(defun timelog-make-subproject-list ()
  "Make a list of subprojects from the `timelog-file'.

Sets `timelog-last-subproject'.  A subproject is everything you specify, when
you log out, i.e. what you've been working on."
  (interactive)
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))
    ;; make the list (search for anything that comes after a time string at BOL
    ;; and before EOL or EOF)
    (while
        (re-search-forward
;; "^o [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] [0-9][0-9]:[0-9][0-9] "
         "^o [0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+ "
                                        ;"o 2000/10/17 12:34 " at BOL
         nil t)
      (add-to-list 'timelog-subproject-list
                   (buffer-substring (match-end 0) (search-forward-regexp
                                                    "$"))))
    (setq timelog-last-subproject (car timelog-subproject-list))
    (bury-buffer)))


(defun timelog-reread-logfile ()
  "Read the `timelog-file' again.

Make new `timelog-project-list' and `timelog-subproject-list'."
  (interactive)
  (setq timelog-project-list ()
        timelog-subproject-list ())
  (timelog-make-project-list)
  (timelog-make-subproject-list))


(defun timelog-read-project (project)
  "Make a list of all entries for PROJECT and return it.

PROJECT should be an existing project in `timelog-file'.  Each entry consists of
2 lines."
  (interactive (list
                (completing-read "Project to read: " (mapcar 'list timelog-project-list)
                                 nil    ; no predicate
                                 t)))   ; require-match
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))
    ;; Search for project, then search backward for "^i" and forward for "^o .*$"
    ;; (end of a line that starts with "o " and take the buffer-substring between
    ;; these two points.  Append it to a list.

    (let ((project-list ()))
      (while
          (re-search-forward project nil t) ; search for the project
        (setq project-list
              (append                   ; we append (list + new-element) ->
                                        ; don't need nreverse
               project-list
               (list (buffer-substring
                      (search-backward-regexp "^i")
                      (search-forward-regexp "^o .*$"))))))

      (bury-buffer)

      ;; return what we came here for (Thanks to Jeff <jeff@dvns.com> for
      ;; explaining nreverse and for the save-excursion.)
      project-list)))


(defun timelog-read-subprojects (project)
  "Make a list of all subprojects of PROJECT.  Return the list.

PROJECT should be an existing project in `timelog-file'."
  (interactive (list
                (completing-read "Read subprojects of which project: "
                                 (mapcar 'list timelog-project-list)
                                 nil    ; no predicate
                                 t)))   ; require-match
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))

    ;; Search for a project, then search forward for sth. like "o 2000/12/12
    ;; 12:23 ", and then search forward for EOL.  Add the string between these
    ;; two points to a list.

    (let ((subproject-list ()))
      (while
          (re-search-forward project nil t) ; search for project
        (add-to-list 'subproject-list
                     (buffer-substring
                      (search-forward-regexp
                       "^o [0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+ ")
                      (search-forward-regexp "$"))))

      (bury-buffer)

      ;; return what we came her for
      (nreverse subproject-list))))


;; This is for you, Jeff <jeff@dvns.com>
(defun timelog-read-days (project)
  "Make a list of all days worked on PROJECT.  Return the list.

PROJECT should be an existing project in `timelog-file'."
  (interactive (list
                (completing-read "Read days worked on which project: "
                                 (mapcar 'list timelog-project-list)
                                 nil    ; no predicate
                                 t)))   ; require-match
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))

    ;; Search for a project, then search forward for sth. like "o 2000/12/12
    ;; 12:23 ", and then search forward for EOL.  Add the string between these
    ;; two points to a list.

    (let ((days-list ()))
      (while
          (re-search-forward project nil t) ; search for project
        (beginning-of-line)
        (search-forward-regexp
         "[0-9]+/[0-9]+/[0-9]+")
        (add-to-list 'days-list
                     (match-string 0))
        (forward-line 1))

      (bury-buffer)

       ;; return what we came her for
      (nreverse days-list))))


(defun timelog-delta-time ()
  "Return the time spent on the subproject point is on.

Time is given in minutes.  If dates for log-in and log-out are different, assume
that sb.  worked after midnight and adjust the result.  Do not assume that
anybody works more than 24 hours.  Return the time as a number."
  (interactive)
  
  ;; find the date and time strings
  (search-backward-regexp "^i ")
  
  (let*
      ((date-1 (progn
                 (search-forward-regexp
;;                "[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]")
                  "[0-9]+/[0-9]+/[0-9]+")
                 (match-string 0)))
       (time-1 (progn
                 (search-forward-regexp
;;                "[0-9][0-9]:[0-9][0-9]")
                  "[0-9]+:[0-9]+")
                 (match-string 0)))
       (date-2 (progn
                 (search-forward-regexp
;;                "[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]")
                  "[0-9]+/[0-9]+/[0-9]+")
                 (match-string 0)))
       (time-2 (progn
                 (search-forward-regexp
;;                "[0-9][0-9]:[0-9][0-9]")
                  "[0-9]+:[0-9]+")
                 (match-string 0)))
       (delta (-
               (timelog-time-to-number time-2)
               (timelog-time-to-number time-1))))

    ;; return delta.  if date-1 and date-2 are not string=, add (* 24 60) 1440
    ;; first.

    (if (not (string= date-1 date-2))
        (+ delta 1440)
      delta)))


(defun timelog-time-of-subproject (project subproject)
  "Return the overall time spent on PROJECT's SUBPROJECT in minutes."
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))

    ;; Search for project on one and subproject on the next line.  Extract the
    ;; date strings from either line and compare.  (If they are not string= we
    ;; assume that sb. worked until after midnight

    (let ((time 0))
      (while
          (re-search-forward
           (concat "^i .*" project "\no .*" subproject)
           nil t)
        (setq time
              (+ time
                 (timelog-delta-time))))

      (bury-buffer)

      time)))

(defun timelog-time-of-project (project)
  "Return the overall time spent on PROJECT in minutes."
  ;; Create a list of all subprojects for project, then a list of times for
  ;; these subprojects.  Then add the times in this list.  Not elegant, but it works.
  (let*
      ((subprojects (timelog-read-subprojects project))
       (subproject-times
        (mapcar
         '(lambda (arg) (timelog-time-of-subproject project arg))
         subprojects))                  ;list of times for each subproject
       ;; there must be some other way to add a list of numbers
       (project-time
        ;; NEVER use eval, Thomas!
        ;;(eval (append '(+) subproject-times)))) ; (eval (+ 1 2)) -> result
        (apply '+ subproject-times)))
    ;; return what we came for
    project-time))

;; The name of this function is not very good
(defun timelog-time-of-day (project day)
  "Return the overall time spent on PROJECT on DAY in minutes."
  
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))

    ;; Search for a line with date and project.
    (let ((time 0))
      (while
          (re-search-forward
           (concat "^i " day ".*" project)
           nil t)
        (setq time
              (+ time
                 (timelog-delta-time))))

      (bury-buffer)
      time)))


(defun timelog-hourly-rate-of-project (project amount)
  "Return the hourly rate for PROJECT if you get AMOUNT for the whole thing.

This could be useful, if you don't get paid by the hour."
  (interactive
   (list
    (completing-read (format "Project (default is %s): "
                             timelog-last-project)
                     (mapcar 'list timelog-project-list)
                     nil                ; no predicat
                     t                  ; require-match
                     nil                ; no initial input
                     nil                ; no history
                     timelog-last-project) ; default
       (string-to-number (read-string "Amount: "))))

  ;; error check
  ;; timelog cannot summarize a project if you're logged in to it
  (if (and (string= project timelog-last-project)
           (string= "i" timelog-last-event))
      (error "You must log out first"))

  (/                                    ; divide
   amount                               ; amount by
   (/ (timelog-time-of-project project) 60.0))) ; hours worked on project


;;; SUMMARY FUNCTIONS
 
;; timelog-summary-include-subprojects, timelog-summary-include-logfile,
;; timelog-summary-include-days, and timelog-summary-include-rates control what
;; functions are called from timelog-summarize-project.

(defun timelog-summary-insert-project-time (project)
  "Insert the time worked on PROJECT."

  (let ((project-time (timelog-time-of-project project)))
    (insert (timelog-number-to-time project-time)) ; insert project-time
    (when timelog-summary-include-rates ; insert rates
      (insert " (" timelog-currency-string " "
              (timelog-number-to-currency-string
               (/
                (* timelog-hourly-rate project-time)
                60.0))
              ")"))))


(defun timelog-summary-insert-subprojects (project)
  "Insert a summary of the subprojects of PROJECT in the timelog summary buffer."
  (let* ((subprojects (timelog-read-subprojects project))
         (subproject-times
          (mapcar
           '(lambda (arg) (timelog-time-of-subproject project arg))
           subprojects))                ;list of times for each subproject
         ;; there must be some other way to add a list of numbers

         ;; find the longest string in subprojects (compute length of each
         ;; subproject, sort the list and get the car
         (longest-string                ; lenght of long. str. in subproj. + 2
          (+ 2
             (car (sort
                   (mapcar 'length subprojects)
                   '>)))))

    (while subprojects
      (insert
       (format (concat "%-" (number-to-string longest-string) "s")
               (car subprojects))       ; subproject + a lot of spaces
       (format "%5s" (timelog-number-to-time (car subproject-times))))  ; " 2:34"

      ;; this is for you, montrealf1@hotmail.com
      (when timelog-summary-include-rates
        (insert " (" timelog-currency-string " " ; (US-$ 123.45)
                (timelog-number-to-currency-string
                 (/
                  (* timelog-hourly-rate (car subproject-times))
                  60.0))
                ")"))

      (insert "\n")

      (setq subprojects (cdr subprojects)
            subproject-times (cdr subproject-times)))))


(defun timelog-summary-insert-days (project)
  "Insert a summary of the days you worked on PROJECT."
  (let* ((days (timelog-read-days project))
         (days-times                    ; sounds silly
          (mapcar
           '(lambda (arg) (timelog-time-of-day project arg))
           days)))                      ;list of times for each day

    (while days
      (insert
       (car days) " "
       (format "%6s" (timelog-number-to-time (car days-times))))

      (when timelog-summary-include-rates
        (insert " (" timelog-currency-string " " ; (US-$ 123.45)
                (timelog-number-to-currency-string
                 (/
                  (* timelog-hourly-rate (car days-times))
                  60.0))
                ")"))

      (insert "\n")

      (setq days (cdr days)
            days-times (cdr days-times)))))


(defun timelog-summary-insert-logfile (project)
  "Insert all in `timelog-file' lines for PROJECT."
      (let ((all-entries (timelog-read-project project)))

        (while all-entries
          (insert (car all-entries) "\n")
          (setq all-entries (cdr all-entries)))))



(defun timelog-summarize-project (project)
  "Create a buffer with a summary of PROJECT.

The buffer contains nicely formatted information on how much time was spent on
PROJECT and its subprojects."
  ;; This function mainly calls other functions.  The called functions
  ;; evaluate the timelog-file to compute everything they need.  Some things
  ;; may be computed more than once.  This is absolutely acceptable.  I want to
  ;; keep it as simple as possible and timelog-summary-insert-* functions
  ;; should require no additional args besides PROJECT.
  (interactive
   (list
    (completing-read (format "Project to summarize (default is %s): "
                             timelog-last-project)
                     (mapcar 'list timelog-project-list)
                     nil                ; no predicat
                     t                  ; require-match
                     nil                ; no initial input
                     nil                ; no history
                     timelog-last-project))) ; default
  ;; timelog cannot summarize a project if you're logged in to it
  (if (and (string= project timelog-last-project)
           (string= "i" timelog-last-event))
      (error "You must log out before you summarize this project"))

  ;; By default we only insert: "Timelog summary: Project \n Overall time:
  ;; 12:34"

  ;; prepare the the buffer
  (switch-to-buffer (get-buffer-create
                     (concat "Timelog Summary: " project)))
  (text-mode)

  ;; insert default
  (insert
   "Timelog Summary of \"" project "\"\n\n"
   "Overall Time: ")
  (timelog-summary-insert-project-time project) ; "12:23"
  (insert " \n")

  ;; insert subprojects
  (when timelog-summary-include-subprojects
    (insert "\nSubprojects:\n")
    (timelog-summary-insert-subprojects project))

  ;; insert day-list
  (when timelog-summary-include-days
    (insert "\nDays worked on project:\n")
    (timelog-summary-insert-days project))

  ;; insert lines from log-file
  (when timelog-summary-include-logfile
    (insert "\nLines from log-file\n")
    (timelog-summary-insert-logfile project))

  (goto-char (point-min)))


(defun timelog-make-summary (ask-for-amount)
  "Call `timelog-summary-function' interactively.

When called with prefix arg ASK-FOR-AMOUNT, bind `timelog-hourly-rate' to the
value returned by `timelog-hourly-rate-of-project' before calling
`timelog-summary-function'.  This is meant to be useful if you are not paid by
the hour."

;; The argument reading is strange, but I couldn't think of a better way.  The
;; argument list should be (project &optional amount prefix-arg).  When called
;; interactively with prefix arg, prompt for project and amount, otherwise
;; prompt for project only.

  (interactive "P")
;    (list (interactive "P")
;        (completing-read (format "Project to summarize (default is %s): "
;                                 timelog-last-project)
;                         (mapcar 'list timelog-project-list)
;                         nil           ; no predicat
;                         t                     ; require-match
;                         nil           ; no initial input
;                         nil           ; no history
;                         timelog-last-project))) ; default

  (let ((project
         (completing-read (format "Project to summarize (default is %s): "
                                  timelog-last-project)
                          (mapcar 'list timelog-project-list)
                          nil           ; no predicat
                          t                     ; require-match
                          nil           ; no initial input
                          nil           ; no history
                          timelog-last-project))) ; default
    (if ask-for-amount
        ;; call with new value for hourly-rate
        (let ((timelog-hourly-rate
               (funcall 'timelog-hourly-rate-of-project
                        project
                        (string-to-number (read-string "Amount: ")))))
          (funcall timelog-summary-function project)) ; is funcall the right
                                        ; thing?
      ;; call with nothing special
      (funcall timelog-summary-function project))))


;; customized-summary
(defun timelog-my-summarize-project (project)
  "Create a buffer with a summary of PROJECT.
Personal version of Thomas Gehrlein."

  (interactive
   (list
    (completing-read (format "Projekt (%s): " ; with default
                             timelog-last-project)
                     (mapcar 'list timelog-project-list)
                     nil                ; no predicat
                     t                  ; require-match
                     nil                ; no initial input
                     nil                ; no history
                     timelog-last-project))) ; default

  ;; timelog cannot summarize a project if you're logged in to it
  (if (and (string= project timelog-last-project)
           (string= "i" timelog-last-event))
      (error "You must log out before you summarize this project"))

  ;; prepare buffer
  (switch-to-buffer (get-buffer-create
                     (concat "Timelog Summary: " project)))
  (erase-buffer)
  (text-mode)

  ;; insert default
  (insert
   project "\n\n"
   "Gesamtzeit:       ")
  (let ((timelog-summary-include-rates nil))
    (timelog-summary-insert-project-time project))
  (insert
   " Stunden\n"
   "Stundensatz:      "
   (timelog-number-to-currency-string timelog-hourly-rate) ; 12,95
   " " timelog-currency-string "/Stunde\n"      ; " DM/Stunde" DM/h
   "Zeilenzahl:       \n"
   "Zeilenpreis:      DM/Zeile\n"
   "Gesamtbetrag:     "                 ; overall amount
   (timelog-number-to-currency-string (* timelog-hourly-rate
                                         (/ (timelog-time-of-project project)
                                            60.0))) ; min -> hours
                                         
   " "
   timelog-currency-string
   "\n"
   "Anmerkungen:\n\n")

  ;; insert subprojects
  (when timelog-summary-include-subprojects
    (insert "\nTeilprojekte:\n")
    (timelog-summary-insert-subprojects project))

  ;; insert day-list
  (when timelog-summary-include-days
    (insert "\nArbeitstage:\n")
    (timelog-summary-insert-days project))

  ;; insert lines from log-file
  (when timelog-summary-include-logfile
    (insert "\nEinträge aus der Log-Datei\n")
    (timelog-summary-insert-logfile project))

  (goto-char (point-min)))



;;; adding information to the timelog-file

(defun timelog-in (project)
  "Log in, recording the current time and the PROJECT you intend to work on."
  ;;read project.  timelog-project-list ist used for completion.  Initial input
  ;;is either the first element of timelog-project list.
  (interactive (progn
                 ;;avoid logging in twice
                 (if (string= timelog-last-event "i")
                     (error "You're already logged in"))
                 (list
                  (completing-read
                   (format "Project to work on (default is \"%s\"): "
                           timelog-last-project)
                   (mapcar 'list timelog-project-list) ;list for
                                        ;completion
                   nil                  ; no predicte
                   nil                  ; no require-match
                   nil                  ; no initial input
                   nil                  ; no history
                   timelog-last-project)))) ; default


  (save-excursion
    (find-file timelog-file)
    (goto-char (point-max))
    ;; insert sth. like "i 2000/10/17 12:34 some project"
    (insert "i "                        ; "i" means "in"
            (format-time-string timelog-time-string)
            " "
            project
            "\n")
    
    ;; update variables: timelog-last-event, timelog-project-list
    (setq timelog-last-event "i"
          timelog-last-project project
          timelog-project-list (add-to-list 'timelog-project-list project))
    (bury-buffer)))

(defun timelog-out (subproject)
  "Log out, recording the current time and the SUBPROJECT you worked on."
  (interactive (progn
                 ;;avoid logging out, if you're not logged in
                 (if (string= timelog-last-event "o")
                     (error "You're not logged in"))

                 (list
                  (completing-read
                   (format "%s: Subproject you worked on (default is \"%s\"): "
                           timelog-last-project
                           timelog-last-subproject) ;prompt
                   (mapcar 'list timelog-subproject-list) ;list for completion
                   nil                  ; no predicte
                   nil                  ; no require-match
                   nil                  ; no initial input
                   nil                  ; no history
                   timelog-last-subproject)))) ;default

  (save-excursion
    (find-file timelog-file)
    (goto-char (point-max))
    ;; insert sth. like "o 2000/10/17 12:34 some subproject"
    (insert "o "                        ; "o" means "out"
            (format-time-string timelog-time-string)
            " "
            subproject
            "\n")

    ;; message
    (when timelog-out-with-message
      (message "You worked %s hours."
               (timelog-number-to-time (timelog-delta-time))))

    ;; update variables: timelog-last-event, timelog-subproject-list
    (setq timelog-last-event "o"
          timelog-last-subproject subproject
          timelog-subproject-list (add-to-list 'timelog-subproject-list
                                               subproject))
    (bury-buffer)))


;; stolen from timeclock.el
;; Add something to save timelog-file
(defun timelog-query-out ()
  "Ask the user before clocking out.

This is a useful function for adding to `kill-emacs-hook'."
  (if (and (string= timelog-last-event "i")
           (y-or-n-p "You're currently clocking time, clock out? "))
      (timelog-out
       ;; read the arg for timelog-out
       (completing-read
        (format "%s: Subproject you worked on (default is \"%s\"): "
                timelog-last-project
                timelog-last-subproject) ;prompt
        (mapcar 'list timelog-subproject-list) ;list for completion
        nil                             ; no predicte
        nil                             ; no require-match
        nil                             ; no initial input
        nil                             ; no history
        timelog-last-subproject))))     ;default


(defun timelog-change (subproject project)
  "Log out and then log in again.

Useful when you want to start on a new SUBPROJECT or PROJECT."
  (interactive (progn

                 ;; make sure, you're logged in
                 (if (string= timelog-last-event "o")
                     (error "You're not logged in"))

                 (list
                  ;; read subproject
                  (completing-read
                   (format "%s: Subproject you worked on (default is \"%s\"): "
                           timelog-last-project
                           timelog-last-subproject) ;prompt
                   (mapcar 'list timelog-subproject-list) ;list for completion
                   nil                  ; no predicte
                   nil                  ; no require-match
                   nil                  ; no initial input
                   nil                  ; no history
                   timelog-last-subproject) ;default
                  ;; read project
                  (completing-read
                   (format "Project to work on (default is \"%s\"): "
                           timelog-last-project)
                   (mapcar 'list timelog-project-list) ;list for
                                        ;completion
                   nil                  ; no predicte
                   nil                  ; no require-match
                   nil                  ; no initial input
                   nil                  ; no history
                   timelog-last-project)))) ; default

  ;; The log-in time may be one minute after the log-out time, since
  ;; format-time-string is called twice.  If that is a real problem, something
  ;; should be done about it.  (It happened to me the other day.)
  (timelog-out subproject)
  (timelog-in project))



;;; Deleting information from the timelog-file
(defun timelog-delete-project (project)
  "Delete a PROJECT from the timelog file.

Use with care.  After a project is deleted, it is gone.  So better make sure to
summmarize the project before you delete it."
  (interactive (list
                (completing-read "Project to delete: "
                                 (mapcar 'list timelog-project-list)
                                 nil    ; no predicat
                                 t)))   ; require-match
  (save-excursion
    (find-file timelog-file)
    (goto-char (point-min))

    (while
        (re-search-forward project
                           nil
                           t)
      (delete-region
;;       (search-backward-regexp "^i [0-9][0-9][0-9][0-9]/[0-9][0-9]")
       (search-backward-regexp "^i [0-9]+/[0-9]+")
       (+ 1
;;        (search-forward-regexp "^o [0-9][0-9][0-9][0-9]/.*$"))))
          (search-forward-regexp "^o [0-9]+/.*$"))))
    
    ;; update
    (timelog-reread-logfile)

    (bury-buffer)))


;;; timelog utility functions

(defun timelog-time-to-number (time)
  "Return the number of minutes of a TIME string like \"12:34\"."
  (interactive "sTime string (hh:mm): ")

  (let* ((hh
          (string-to-int (substring time 0 2))) ;hh
         (mm
          (string-to-int (substring time 3 5)))) ;mm

    (+ mm                               ; mm + (hh * 60)
       (* 60 hh))))

(defun timelog-number-to-time (number)
  "Return NUMBER minutes as a string like \"12:23 \" or \"1:23\".
Hours are not zero-padded."
  (interactive "nHow many minutes: ")

  (let*
      ((hours (int-to-string (/ number 60)))
       ;; hh (since we're using ints, an int is returned)
       (mins (int-to-string (mod number 60)))
       (mins (if (= (length mins) 1)    ;pad a "0", if length = 1
                 (concat "0" mins)
               mins)))
    (concat hours ":" mins)))

(defun timelog-number-to-currency-string (amount)
  "Format AMOUNT as \"1234.44\" or \"12,00\"."
  (let* ((amount-string
          (number-to-string (round (* 100 amount)))) ; 123456.7812 ->12345678
         (dollars (substring amount-string ; 123456
                             0
                             (- (length amount-string) 2)))
         (cents (substring amount-string ; 78
                           (- (length amount-string) 2))))
    (concat dollars
            timelog-currency-separator-string
            cents)))                    ; 123456.78


;;; Prepare everything for use
(timelog-make-project-list)
(timelog-make-subproject-list)
(add-hook 'kill-emacs-hook 'timelog-query-out) ; don't kill emacs before you
                                               ; logged out


(provide 'timelog)

;;; timelog.el ends here