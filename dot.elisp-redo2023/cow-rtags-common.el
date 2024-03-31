;;; Support for using rtags for C++

(use-package company :ensure t)
(use-package rtags
  :ensure t
  :config (push 'company-rtags company-backends)
  :config (setq rtags-completions-enabled t)
  :config (rtags-enable-standard-keybindings)
  :config (setq rtags-display-result-backend 'default))
(use-package company-rtags  :ensure t)
(use-package flycheck-rtags :ensure t)

(defun cow-rdm-select-a-compile-commands-json (path)
  "Load a compile_commands.json file and start RDM server.

The file at PATH will be used for starting and controlling RDM.
The pathname to the compile_commands.json will be the basis for
the name of a subdirectory in
_cow-rdms-parent-dir (i.e. ~/.rdm-servers).
  "
  (let*
      ((full-path  (expand-file-name path))
       (plain-name (file-name-nondirectory path)))
    (if (not (string-equal plain-name "compile_commands.json"))
	(error "Unsupported file %s" plain-name))

    ;; Stash away location of of compile_commands.json.  All other
    ;; project locations are based off of that.    
    (setq _cow-rdm-compile-commands-json-path full-path)
    (make-directory (_cow-rdm-server-dir) t)
                        
    ;; kill any previous cow-created RDM servers
    ; TODO figure out how and figure out if we really want to and need
    ; to do that.

    (message "About to start RDM")
    ;; Start rdm
    (let*
        ((job-count   (max 1 (- (system-cores :physical t) 1)))
         (excludes    (format " --exclude-filter %s"
                              (concat "\""
                                      "*/tmp/bsr2-builds/*;"
                                      "*/CMakeFiles/*;"
                                      "*/cmake*/Modules/*;"
                                      "*/conftest.c*;"
                                      "/tmp/*;"
                                      "/private/tmp/*;"
                                      "/private/var/*\"")))
         (cmd-line    (concat (rtags-executable-find "rdm")
                              (format " --socket-file %s" (_cow-rdm-server-socket))
                              (format " --data-dir %s"    (_cow-rdm-server-datadir))
                              (format " --log-file %s"    (_cow-rdm-server-log-file))
                              " --silent"
                              " --rp-nice-value 1 "
                              excludes
                              ;; " --watch-sources-only"
                              ;; " --daemon"
                              (format " --job-count %d"  job-count)))
         (unique-name         (replace-regexp-in-string "/" "_"
                                                        (_cow-rdm-server-dir)))
         (rdm-process (start-file-process-shell-command
                             (format "RTags-%s" unique-name)
                             (format "*rdm-%s*" unique-name)
                             cmd-line)))
      (message "RDM started.\n")
      (set-process-query-on-exit-flag rdm-process nil)
      (set-process-sentinel           rdm-process 'rtags-sentinel))

    ;; Wait until rdm is running and responsive
    (dotimes (i 5)
      (let ((running    (_cow-rdm-server-running))
            (responsive (_cow-rdm-server-responsive)))
        (when (not (and running responsive))
          (message "Waiting for RDM server to start")
          (sleep-for 1))))
    (unless (_cow-rdm-server-responsive)
      (error "Could not start RDM server"))
    (sleep-for 1) 
    
    ;; Tell RDM to load up the compile_commands.json and start indexing
    (with-temp-buffer                                                              
      (message "%s: %s" path (_cow-rdm-server-socket))
      (rtags-call-rc "--socket-file" (_cow-rdm-server-socket)
                     "-J" (file-name-directory full-path)))

    ;; Connect Emacs to the RDM  server.
    (setq rtags-socket-file (_cow-rdm-server-socket))

    ;; F9-f will find and open a file that rdm is aware of. 
    (global-set-key [f9 ?f] 'rtags-find-file)

    ;; Create a README.txt file in the server directory
    (let* ((filename (concat (_cow-rdm-server-dir) "/README.txt")))
      (unless (file-exists-p filename)
        (with-temp-file filename (insert (message "Project: %s\n" path)))))

    _cow-rdm-compile-commands-json-path))


(defun _cow-rdm-server-not-running ()
  (not
   (remove-if-not
    #'(lambda (pid)
        (let*
            ((info   (process-attributes pid))
             (cmd    (cdr (assoc 'comm info)))
             (args   (cdr (assoc 'args info)))
             (exe    (rtags-executable-find "rdm"))
             (is-rdm (and (string-equal cmd "rdm")
                          (string-prefix-p exe args)))
             (result (and is-rdm 
                          (string-match (regexp-quote (_cow-rdm-server-socket)) args))))
          result))
    ;; List of all processes with pid, comm, args, etc. 
    (list-system-processes))))

(defun _cow-rdm-server-running () (not (_cow-rdm-server-not-running)))

(defun _cow-rdm-server-responsive () 
  (with-temp-buffer (rtags-call-rc "--socket-file" (_cow-rdm-server-socket)
                                   "--timeout=1000"
                                   "--is-indexing")))
    
;; 
;; Define where the rdm servers should store data
;; and make sure that directory exists.
;; 
(defconst _cow-rdms-parent-dir (expand-file-name "~/.rdm-servers")
  "Parent directory of directories in which rdm will store data")
(make-directory _cow-rdms-parent-dir t)

(defconst _cow-rdm-dir-prefix (concat _cow-rdms-parent-dir "/cow-"))
    
(defvar _cow-rdm-compile-commands-json-path nil
  "Location of the currently active compile-commands.json file")

(defun _cow-rdm-server-dir ()
  "Return the full path the directory which current RDM server is using

e.g. 
  /home/craig/.rdm-servers/cow-_home_craig_sandboxes_work_SystemsSrc_
"
  (concat _cow-rdm-dir-prefix 
	  (replace-regexp-in-string
	   "/" "_"
	   (expand-file-name (file-name-directory _cow-rdm-compile-commands-json-path)))))

(defun _cow-rdm-server-datadir ()
  "Location where rdm will store its database"
  (concat (_cow-rdm-server-dir) "/rtags"))

(defun _cow-rdm-server-log-file ()
  "Location where rdm logs status"
  (concat (_cow-rdm-server-dir) "/progress"))

(defun _cow-rdm-server-socket ()
  "Path to the RDM socket"
  (concat (_cow-rdm-server-dir) "/rdm_socket"))

;; system-cores let's us know how many CPUs we have for parallel builds
(if load-file-name
    (let* ((fname (concat (file-name-directory load-file-name)
			  "3rdParty/system-cores.el")))
      (load fname)
      (require 'system-cores)))

(provide 'cow-rtags-common)
