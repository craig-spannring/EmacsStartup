;;;
;;; Copyright 2018 Craig Spannring
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of Craig Spannring may not be used to endorse or promote
;;;    products derived from this software without specific prior
;;;    written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY CRAIG SPANNRING ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL CRAIG SPANNRING BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;

;;;
;;;  This set of functions provides and rtags-based IDE support for to
;;;  the msvc_functions. 
;;;
;;; Internals-
;;;
;;;    Generally speaking, the top-level functions combines a
;;;    compile_commands.json and a .depend file to create the data
;;;    structures used by the  msvc_functions.el
;;;  
;;;    The basic strategy is to create one rdm server for each project
;;;    that we load (using the msvc-load-project) and tell the rtags
;;;    system to use that as the rdm server.
;;;
;;;    Basic mechanism for creating the rdm server is
;;;       SBROOT=$HOME/.rdm_servers/<some name derived from project location>
;;;       mkdir -p $SBROOT/.rtags
;;;       rdm --socket-file=$SBROOT/.rtags/rdm_socket \
;;;           --data-dir=$SBROOT/.rtags/rtags \
;;;           --log-file=$SBROOT/.rtags/rdm.log \
;;;           --silent \
;;;           --watch-sources-only \
;;;           --job-count=$NCORES
;;;       


(install-and-require-packages '(system-cores))

(defcustom cts-rtp-rdmservers-dir
  (expand-file-name "~/.rdm-servers")
  "Directory where the rdm server subdirectories live"
  :group 'rtags-current-project
  :type 'string)

(defun cts-rtp-switch-project (project-path)
  (interactive "D")
  (setq rtags-socket-file (cts-rtp--rdmserver-socket project-path)))

(defun cts-rtp--load-compile-commands (project-path)
  "Tell rdm to load the compile_commands.json file"
  (with-temp-buffer
    (rtags-call-rc "--socket-file" (cts-rtp--rdmserver-socket project-path)
                 "-J" project-path)))

(defun cts-rtp-start-rdmserver-unless-running (project-path)
  "Start an rdm server for the project
   PROJECT-PATH   fully qualified path of directory containing .rtags-config file
  "

  (when (not (cts-rtp--is-server-running project-path))
    ;; Make sure the directory for the server exists
    (make-directory (cts-rtp--rdmserver-dir project-path) t)

    ;; Start the rdm process 
    (let* ((hash (cts-rtp--projpath-hash project-path))
           (rtags-rdm-process (start-file-process-shell-command
                              (format "RTags-%s" hash)
                              (format "*rdm-%s*" hash)
                              (cts-rtp--rdm-cmdline project-path))))
      (set-process-query-on-exit-flag rtags-rdm-process nil)
      (set-process-sentinel rtags-rdm-process 'rtags-sentinel))
    ;; Create a readme file in the rdm directory
    (let* ((filename (concat (cts-rtp--rdmserver-dir project-path) "README.txt")))
          (unless (file-exists-p filename)
              (with-temp-file filename (insert (message "Project: %s\n" project-path)))))))


(defun cts-rtp--rdm-cmdline (project-path)
  "Return the command to run rdm for the project"
  ;; rdm                                      
  ;; --socket-file=$SBROOT/.rtags/rdm_socket
  ;; --data-dir=$SBROOT/.rtags/rtags
  ;; --log-file=$SBROOT/.rtags/rdm.log
  ;; --silent
  ;; --watch-sources-only
  ;; --job-count=$NCORES
  (concat (rtags-executable-find "rdm")                      
          (format " --socket-file %s" (cts-rtp--rdmserver-socket project-path))
          (format " --data-dir %s" (cts-rtp--rdmserver-datadir project-path))
          (format " --log-file %s" (cts-rtp--rdmserver-logfile project-path))
          " --silent"
          " --rp-nice-value 1 "
          ; " --watch-sources-only"
          ; " --daemon"
          (format " --job-count %d" (system-cores :physical))))

(defun cts-rtp--is-server-running (project-path)
  ;; Look through all the processes for one that has the same command
  ;; line as the rdm for this project would have.
  (let* ((found nil)
         (cmdline (cts-rtp--rdm-cmdline project-path)))
    
    (dolist (pid (list-system-processes) found)
      (let* ((attrs (reverse (process-attributes pid)))
             (cmd   (cdr (assoc 'args attrs))))
        (setq found (or found (string-equal cmdline cmd)))))
    found))
    
(defun cts-rtp--is-server-responsive (project-path)
  (let* ((sock-path (cts-rtp--rdmserver-socket project-path)))
    (with-temp-buffer (rtags-call-rc "--socket-file" sock-path
                                     "--timeout=1000"
                                     "--is-indexing"))))
    

(defun cts-rtp--projpath-hash (project-path)
  (md5 project-path nil nil 'raw-text))

(defun cts-rtp--rdmserver-dir (project-path)
  "fully qualified name of the rdm server directory for socket file for project-path

  note- the returned path will have a trailing directory seperator

  PROJECT-PATH   fully qualified path of directory containing .rtags-config file
  "

  (concat
   (file-name-as-directory cts-rtp-rdmservers-dir)
   (file-name-as-directory (cts-rtp--projpath-hash project-path))))


(defun cts-rtp--rdmserver-socket (project-path)
  "fully qualified name of the rdm server socket file for project-path

  PROJECT-PATH   fully qualified path of directory containing .rtags-config file
  "

  (concat (cts-rtp--rdmserver-dir project-path) "rdm_socket"))

(defun cts-rtp--rdmserver-datadir (project-path)
  "fully qualified name of the rdm server data direcotry for project-path

  PROJECT-PATH   fully qualified path of directory containing .rtags-config file
  "

  (concat (cts-rtp--rdmserver-dir project-path) "rtags"))


(defun cts-rtp--rdmserver-logfile (project-path)
  "fully qualified name of the rdm server log file for project-path

  PROJECT-PATH   fully qualified path of directory containing .rtags-config file
  "
  (concat (cts-rtp--rdmserver-dir project-path) "rdm.log"))

(defun cts-rtp-all-files (project-path)
  (message "rdm status %s" (cts-rtp--is-server-responsive project-path))
  )

(provide 'rtags_project)
