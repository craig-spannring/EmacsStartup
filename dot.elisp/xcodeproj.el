#!/usr/bin/emacs --script
;;;;
;;;; Copyright 2012 Craig Spannring
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 3. The name of Craig Spannring may not be used to endorse or promote
;;;;    products derived from this software without specific prior
;;;;    written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY CRAIG SPANNRING ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL CRAIG SPANNRING BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.
;;;;

(require 'cl)

(defun _xcp_load-pbx-file-into-new-buffer (pbx-file)
  "Load a .pbxproj project file into a new buffer
   
   Return-  The buffer that contains the contents of the pbx file. 
   "
  (let*  ((old-buf (current-buffer))
          (proj-buf  (generate-new-buffer "*xcodeproj-parse*")))
    (set-buffer proj-buf)
    (message "pbx-file is %s" pbx-file)
    (insert-file-contents pbx-file nil nil nil t)
    (set-buffer old-buf)
    proj-buf))


(defun _xcp_pbx-file-from-project-dir (projdir)
  "Given the name of the <proj>.xcodeproj directory, return the .pbxfile name

   This return the fully qualified name of the the project
   file in the project directory.  This assumes the file is
   always named 'project.pbxproj'.  

   Question- Is that always true in light of the new workspace
   paradigm in xcode4? "
  (file-truename (concat (file-name-as-directory projdir) 
                         "project.pbxproj")))


(defun _xcp_find-start-of-containing-group_r (poi pos)
  "return the location of the opening bracket of the innermost group
  that contains pos"

  (let* ((old-point (point)))
    (goto-char pos)
    (search-backward "{")
    (forward-sexp)
    (cond ((< (point) poi) 
           (backward-sexp)
           (let ((new-pos (point)))
             (goto-char old-point)
             (_xcp_find-start-of-containing-group_r poi new-pos)))
          (t 
           (backward-sexp)
           (let ((result (point)))
             (goto-char old-point)
             result)))))

(defun _xcp_find-start-of-containing-group (pos)
  "return the location of the opening bracket of the innermost group
  that contains pos"
  (_xcp_find-start-of-containing-group_r pos pos))


(defun _xcp_find-end-of-containing-group (pos)
  "return the location of the closing bracket of the innermost group
  that contains pos"
    (let* ((old-point (point))
           (start-point (_xcp_find-start-of-containing-group pos)))
      (goto-char start-point)
      (forward-sexp)
      (let ((result (point)))
        (goto-char old-point)
        result)))

(defun _xcp_get-next-normal-declaration (min-pos max-pos)
  "" 
  (let* ((old-point (point))
         (old-fold  case-fold-search)
         ;; regexp that (more or less) picks out declarations in project file
         (pattern   "\\<[[:alnum:]]+[[:blank:]]*=[[:blank:]]*\"?[[:alnum:][:blank:]_.]+\"?;"))
    
    (setq case-fold-search nil)
    (goto-char min-pos)
    (let* ((name-end   (search-forward-regexp  pattern max-pos t))
           (name-start (search-backward-regexp pattern min-pos t))
           (line       (cond (name-end  (buffer-substring name-start name-end))
                             (t         nil)))
           (components (cond (line
                              (split-string line "[[:blank:]]=[[:blank:]]\"?"))
                             (t     nil))))
      (goto-char old-point)         ; restore point
      (setq case-fold-search nil)   ; restore case-sensitivity settings
      (cond (line (list name-end
                        (car components) 
                        ;; remove trailing " from the value portion 
                        (car (split-string (cadr components) "\"?;"))))
            (t    nil)))))

(defun _xcp_get-normal-declations-in-block (min-pos max-pos)
  "return a list of the declarations in this block"
  (let*  ((decl (_xcp_get-next-normal-declaration min-pos max-pos)))
    (cond (decl  (cons (cdr decl) 
                       (_xcp_get-normal-declations-in-block (car decl) max-pos)))
          (t     nil))))

(defun foo ()
  (interactive)
  (save-excursion
  ;; (message "%s"
  ;;          (_xcp_get-declations-in-block
  ;;           (_xcp_find-start-of-containing-group (point))
  ;;           (_xcp_find-end-of-containing-group (point))))
    ;;(message "%s" (_xcp_positions-of-matches "isa = XCBuildConfiguration"))
    (message "xcp_get-build-config-list -> %s" (xcp_get-build-config-list "/tmp/foo.pbxproj"))
    (print (xcp_get-build-config-list "/tmp/foo.pbxproj"))
    (message "stringp on first item %s" (stringp (car (xcp_get-build-config-list "/tmp/foo.pbxproj"))))
  ))

(defun xcp_get-filelist-from-proj (pbx-file)
  "Parse the xocde project and return a list of files. 
      
  Parameters:
     PBX-FILE   fully qualified path of the project.pbxproj file. 

  Note- This currently only handles xcode4 project files.
  "
  

  (let*  ((proj-buf  (_xcp_load-pbx-file-into-new-buffer pbx-file)))
    (set-buffer proj-buf)
    (message "pbx-file is %s" pbx-file)
    (insert-file-contents pbx-file nil nil nil t)
    
    
    )
  )

(defun _xcp_positions-of-matches (str)
  (save-excursion
    (let* ((result nil))
      (goto-char (point-min))
      (while (search-forward str nil t)
        (search-backward-regexp str nil t)
        (setq result (cons (point) result))
        (forward-char))
      result)))

(defun xcp_get-build-config-list (pbx-file)
  "Return a list of the build configurations for this project"
  (let* ((proj-buf  (_xcp_load-pbx-file-into-new-buffer pbx-file)))
    (set-buffer proj-buf)
    
    (goto-char (point-min))

    (mapcar (lambda (item) (car (cdr (car item))))
            (mapcar 
             (lambda (pos) 
                      (remove-if-not (lambda (x) (string= "name" (car x)))
                                     (_xcp_get-normal-declations-in-block 
                                      (_xcp_find-start-of-containing-group pos)
                                      (_xcp_find-end-of-containing-group   pos))))
                    (_xcp_positions-of-matches "isa = XCBuildConfiguration")))))

