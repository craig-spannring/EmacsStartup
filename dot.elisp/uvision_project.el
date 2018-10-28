;;;;
;;;; Copyright 2012 Craig Spannring
;;;;
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

;;;
;;; This set of functions provides Keil uvision support to the msvc_functions.
;;;
;;; Internals-
;;;
;;;    Generally speaking, the top-level functions take an xml node
;;;    created by something like
;;;       (car (xml-parse-file "myprog.uvproj")

(defun _uv-get-build-targets (proj-node)
  "Return a list names of build targets in PROJ-NODE

   PROJ-NODE is the topmost xml node of the project.
  "

  ;;
  ;; The uvision XML schema guarantees we always have exactly 1 "Targets" node.
  ;;
  (let* ((targets (xml-get-children
                   (car (xml-get-children proj-node 'Targets))
                   'Target))
         (target-name-nodes (mapcar '(lambda (target)
                                       (xml-get-children target 'TargetName))
                                    targets)))
    (mapcar '(lambda (name-node)
               (caddar name-node)) target-name-nodes)))


(defun uv-get-build-targets-from-file (proj-file)
  "Return a list names of build targets in the project file

   PROJ-NODE is the topmost xml node of the project.
  "
  (_uv-get-build-targets (car (xml-parse-file proj-file))))


(defun uv-get-source-files (proj-file dep-file)
  ;; TODO get the files from the .uvproj file as well
  (uv-get-source-files-from-dep dep-file))


(defun uv-get-source-files-from-dep (dep-file)
  (let ((result nil))
    (with-temp-buffer
      (insert-file-contents dep-file)

      ;; Convert backslashes to forward slashes.
      (goto-char (point-min))
      (while (search-forward "\\" nil t)
        (replace-match "/" nil t))

      (goto-char (point-min))
      (while (search-forward-regexp "^[FI] (" nil t)
        ; (message "str is %s" (buffer-substring (point) (+ 5 (point))))
        (let* ((system-header-path "C:/Keil/") ; TODO lookup in Win32 registry
               (start-of-fname     (point))
               (end-of-fname       (search-forward-regexp ")"))
               (name               (cond (end-of-fname
                                          (buffer-substring start-of-fname
                                                            (- end-of-fname 1)))
                                         (t nil))))
          (when (and name
                     (not (string-prefix-p system-header-path name)))
            (setq result (cons name result))))))
    result))

;;; Older versions of emacs don't have the string-prefix-p function
(when (not (functionp 'string-prefix-p))
  (defun string-prefix-p (str1 str2 &optional ignore-case)
    (let* ((end-pos (length str1)))
      (equal t (compare-strings str1 0 end-pos
                       str2 0 end-pos ignore-case)))))


(provide 'uvision_project)