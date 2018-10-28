;;; dired-cvs.el -- Extensions to dired.
;;; Copyright (C) 1991,1992  Per Cederqvist
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Note that this package is still under development. Comments,
;;; enhancements and bug fixes are welcome.
;;; Send them to ceder@lysator.liu.se.

;;; dired-cvs.el,v 1.4 1992/01/22 17:09:37 ceder Exp

;;; This file works with the dired which says:
;; >DIRED commands for Emacs.  1.4
;; >Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.
;; >Enhanced from Emacs 18.55 dired by Sebastian Kremer <sk@thp.uni-koeln.de>
;;; on the first three lines. It might not work with other versions of dired.

(require 'pcl-cvs)
(require 'dired)
(define-prefix-command 'dired-Control-C-Prefix)

(defvar dired-mode-hook nil)

(setq dired-mode-hook
      (` ((lambda ()
	    (define-key dired-mode-map "\C-c" 'dired-Control-C-Prefix)
	    (define-key dired-mode-map "\C-ca" 'dired-cvs-add)
	    (define-key dired-mode-map "\C-cl" 'dired-mark-cvslog)
	    (define-key dired-mode-map "\C-cr" 'dired-cvs-rename)
	    (define-key dired-mode-map "\C-cd" 'dired-cvs-remove))
	  (,@ dired-mode-hook)
	  (,@ nil))))

(defun dired-mark-cvslog (&optional arg)
  "Compress marked (or next ARG) files."
  (interactive "P")
  (save-window-excursion
    (pop-to-buffer (get-buffer-create cvs-temp-buffer-name))
    (erase-buffer))
  (dired-mark-map-check
   'dired-cvslog arg "%d of %d logging%s failed - type W to see why %s"
   'compress "Cvs log %s ")
  (pop-to-buffer (get-buffer-create cvs-temp-buffer-name)))


(defun dired-cvslog ()
  ;; Return nil for success, offending filename else.
  (let* ((file-to-log (dired-get-filename)))
    (if (save-excursion
	  (beginning-of-line)
	  (looking-at dired-re-sym))
	(progn
	  (dired-log (concat
		      "Attempt to cvs-log a symbolic link:\n"
		      file-to-log))
	  file-to-log)
      (set-buffer (get-buffer cvs-temp-buffer-name))
      (setq default-directory (file-name-directory file-to-log))
      (call-process cvs-program nil t t "log"
		    (file-name-nondirectory file-to-log)))))


(defun dired-cvs-remove ()
  "Remove one file from the repository."
  (interactive)
  (let ((filename (dired-get-filename)))
  (cond
   ((yes-or-no-p (format "Delete %s? " filename))
    (let ((msg (read-string "Log message: ")))
      (delete-file filename)
      (let (buffer-read-only)
	(delete-region (progn (beginning-of-line) (point))
		       (progn (forward-line 1) (point))))
      (pop-to-buffer (get-buffer-create cvs-temp-buffer-name))
      (erase-buffer)
      (setq default-directory (file-name-directory filename))
      (message "Removing from repository...")
      (call-process cvs-program nil t t "remove"
		    (file-name-nondirectory filename))
      (message "Removing from repository...Done."))))))


(defun dired-cvs-add (file-info)
  "Add one file from the repository."
  (interactive "sFile declaration: ")
  (let ((filename (dired-get-filename)))
    (pop-to-buffer (get-buffer-create cvs-temp-buffer-name))
    (erase-buffer)
    (setq default-directory (file-name-directory filename))
    (message "Adding to repository...")
    (call-process cvs-program nil t t "add"
		  "-m" file-info
		  (file-name-nondirectory filename))
    (message "Adding to repository...Done.")))


(defun dired-cvs-rename (new-name)
  "Rename a cvs file."
  (interactive
   (list (read-file-name
	  (format "Rename %s to "
		  (file-name-nondirectory (dired-get-filename)))
	  (file-name-directory (dired-get-filename)))))

  (let* ((old-name (dired-get-filename))
	 (temp-name (make-temp-name (dired-get-filename))))
    (copy-file (cvs-repository-name old-name)
	       temp-name)
    (rename-file old-name new-name)
    ;; cd old-dir; rm old-name; cvs remove old-name; cvs commit -m '' old-name
    ;; cd new-dir; cvs add new-name; cvs commit -m '' new-name
    (shell-command (concat "cd " (file-name-directory old-name)
			   " ; " cvs-program " remove "
			   (file-name-nondirectory old-name)
			   " ; " cvs-program " commit -m ' ' "
			   (file-name-nondirectory old-name)
			   " ; cd " (file-name-directory new-name)
			   " ; " cvs-program " add -m ' ' "
			   (file-name-nondirectory new-name)
			   " ; " cvs-program " commit -fm ' ' "
			   (file-name-nondirectory new-name)))
    (delete-file (cvs-repository-name new-name))
    (rename-file temp-name (cvs-repository-name new-name))
    (dired-remove-entry-all-buffers old-name)
    (dired-rename-visited old-name new-name)))

(defun cvs-repository-name (filename)
  "Get the name of the RCS file that corresponds to FILENAME.
FILENAME should be a full path.
Works by looking in (file-name-directory FILENAME)/CVS.adm/Repository."
  (save-window-excursion
    (set-buffer (find-file-noselect
		 (concat (file-name-directory filename)
			 "CVS.adm/Repository")))
    (goto-char (point-min))
    (let ((path (buffer-substring (point) (progn (end-of-line) (point)))))
      (if (/= ?/ (elt path 0))
	  (let ((root (getenv "CVSROOT")))
	    (if root
		(setq path (concat (file-name-as-directory root)
				   path))
	      (error "Must set CVSROOT environment variable."))))
      (concat (file-name-as-directory path)
	      (file-name-nondirectory filename)
	      ",v"))))
