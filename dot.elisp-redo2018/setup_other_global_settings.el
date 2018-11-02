;;
;; Other global settings for emacs
;;
(setq-default indent-tabs-mode nil)  ;; indent-tabs-mode is local to each 
                                     ;; buffer therefore we must do a 
                                     ;; setq-default to set the default
                                     ;; value for each buffer.
  
(setq scroll-step 1)
(setq blink-matching-paren t)
(setq blink-matching-paren-distance 50000)
(setq next-line-add-newlines nil)


(defun plotwrite ()
  "Write a region to the file /tmp/foo.plt.

The region should contain valid gnuplot data."

  (interactive)
  (let ((plot-filename "/tmp/foo.plt"))
  (cond 
   ((file-exists-p plot-filename)
    (delete-file plot-filename)))
  (write-region (region-beginning) (region-end) plot-filename)))



(defun set-tab-width (w)
   (interactive "nWidth: ")
   (setq tab-width w))



;;
;;   Match parenthesis with key F9-m
;;
(defun emx-match-paren ()
  "Go to the matching parenthesis if on parenthesis.
This function uses the syntax table."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun journal-entry (&optional x)
  "Add a journal entry.  If a prefix argument is given 
then this will add the entry at the point in 
the current buffer.  When no prefix argument is given 
it will open the $HOME/.journal file and add the entry 
at the end of the file.

This binds the C-c C-c key sequence to a function that will 
save the journal entry and bury the .journal buffer"

  (interactive "P")
  (when (not x)
    (let ((cmd (list 'lambda nil '(interactive)
                     (list 'let '(tmp_buf) 
                           '(save-buffer)
                           '(setq tmp_buf (current-buffer))
                           '(condition-case nil
                                (delete-window)
                              (error nil))
                           '(switch-to-buffer "*scratch*")
                           (list 'condition-case nil 
                                 (list 'switch-to-buffer (current-buffer))
                                 '(error nil))
                           '(bury-buffer tmp_buf)))))
      (find-file-other-window "~/.journal")
      (message "cmd is %s" cmd)
      (local-set-key [(control ?c) (control ?c)] cmd))
    (goto-char (point-max)))
  (insert (format "\n\n<-- %s -->\n" (concat (format-time-string "%Y-%m-%d %T ") (user-login-name)))))


(defun add_interruption (guilty)
  "Add an entry to the intterruption log"
  (interactive "sName: ")
  ;; The interruption log is a file of lists, one list per interruption.
  ;; Each list is of the form (guilty_party time reason).  guilty_party
  ;; and reason are strings.  time is a list of three integers in the same
  ;; form as the list returned by (current-time)
  (let (comment-line)
    (find-file-other-window "~/.interruption_log")
    (setq comment-line (point))
    (goto-char (point-min))
    (insert (format "%S\n" (list guilty (current-time) '"")))
    (goto-char (- (point) 3))
    (insert (format "%s-  " (current-time-string)))
    )
)


;; (cond ((not (or
;; 			(string= (system-name) "olympus.bzn.vlt.eds.com")
;; 			(string= (system-name) "giac1.oscs.montana.edu")))
;; 		  (standard-display-european 1)))
;; (load-library "iso-syntax")
;; (load-library "iso-transl")

(when (< emacs-major-version 22)
   (standard-display-european 1))




(cond (window-system (font-lock-mode 1))
      (t             nil))

(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)

;; (load-file "~/.elisp/ada_stuff.el")
;; (load-file "~/.elisp/vos-cm-mode.el")
(load-file "~/.elisp/c_functions.el")


(when (equal system-type 'windows-nt)
  (defun explorer (dir)
    "Open a directory in Explorer"
    (interactive "D")
    (setq dir (expand-file-name dir))
    (cond
     ((xemacs-p)
      nil)
     (t
      (setq dir (replace-regexp-in-string "/" "\\" dir t t))))
    (cond 
     ((string= (substring dir -2 nil) ":\\") nil)
     ((string= (substring dir -1 nil) "\\") (setq dir (substring dir 0 -1))))
    (message "The dir is %s" dir)
    (shell-command-to-string (format "explorer %s" (shell-quote-argument dir)))
    ; (dired-do-shell-command "explorer" nil (list dir))
    ; (dired-do-shell-command "explorer" nil (list dir))
))


(when (equal system-type 'gnu/linux)
  (defun explorer (dir)
    "Open a directory in nautilus"
    (interactive "D")
    (setq dir (expand-file-name dir))

    (cond 
     ((string= (substring dir -2 nil) ":\\") nil)
     ((string= (substring dir -1 nil) "\\") (setq dir (substring dir 0 -1))))
    (message "The dir is %s" dir)
    (shell-command-to-string (format "nautilus %s" (shell-quote-argument dir)))
    ; (dired-do-shell-command "explorer" nil (list dir))
    ; (dired-do-shell-command "explorer" nil (list dir))
))

(when (equal system-type 'darwin)
  (defun explorer (dir)
    "Open a directory in Finder"
    (interactive "D")
    (setq dir (expand-file-name dir))
    (shell-command-to-string (format "open %s" (shell-quote-argument dir)))))


(when (equal system-type 'darwin)
  (setq process-connection-type t))

(when (equal system-type 'windows-nt)
  (setq ispell-program-name "aspell"))

(when (string= (system-name) "SEVILLE")
  (setq ispell-program-name "aspell"))

(when (string= (system-name) "MARS")
  (cond 
   ((boundp 'Info-directory-list)
    (message "The Info-directory-list is %s" Info-directory-list))
   (t (message "The Info-directory-list is not bound")))
  (setq Info-directory-list '("c:/emacs-21.3/info/" "c:/cygwin/usr/local/embedded-arm-cross/info/dir" "c:/cygwin/usr/info" ))
)


(defconst CDefine::KM2MI      0.621371192           " kilometers to miles ")
(defconst CDefine::MEPS2MIPH  2.2369362912          " meters/sec to miles/hour ")
(defconst CDefine::MEPS2KMPH  3.6000                " meters/sec to kilometers/hour")
(defconst CDefine::MIPH2MEPS  (/ 1.0 2.2369362912)  " miles/hour to meters/sec ")
(defconst CDefine::KMPH2MEPS  (/ 1.0 3.6000)        " kilometers/hour to meters/sec ")
(defconst CDefine::M2F        3.280839895           " meters to feet")
(defconst CDefine::M2I        39.37007874 	    " meters to inches ")
(defconst CDefine::PI         3.1415926535897932    " Value of pi")
(defconst CDefine::R2D        (/ 180.0 CDefine::PI) " Scale radians to degrees")
(defconst CDefine::D2R        (/ CDefine::PI 180.0) " Scale degrees to radians")


(defun to-degrees (rad) "convert radian measure to degrees" (* CDefine::R2D rad))
(defun to-radians (degrees) "convert degree measure to radians" (* CDefine::D2R degrees))


(defun tohex (num)
  (format "%x" num))

(defun todec (num)
  (string-to-number num 16))

(defun reverse-case-region ()
  "Swap the case of letters within current region"

  (interactive)

  (save-excursion
    (let ((tmp1 nil)
          (tmp2 nil)
          range
          value
          number
          (my-swap-table ""))
     
      
      (dotimes (number 123 tmp1)
        (setq my-swap-table (concat my-swap-table 
                                    (string (cond ((and (>= number 97) (<= number 122)) (- number 32))
                                                  ((and (>= number 65) (<= number 90))  (+ number 32))
                                                  (t                                   number))))))
      (translate-region (region-beginning) (region-end) my-swap-table))))

(save-place-mode 1)
