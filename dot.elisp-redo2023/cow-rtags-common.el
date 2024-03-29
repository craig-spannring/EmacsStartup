
(use-package rtags
  :ensure t
  :config (setq rtags-completions-enabled t)
  :config (setq rtags-use_helm            t)
  :config (setq rtags-display-result-backend      'helm))
(use-package company-rtags  :ensure t)
(use-package flycheck-rtags :ensure t)

(defun cow-rdm-select-a-compile-commands-json (path)
  "Load a compile_commands.json file.

The file at PATH will be used for starting and controlling RDM.
The pathname to the compile_commands.json will be the basis for
the name of a subdirectory in
_cow-rdms-parent-dir (i.e. ~/.rdm-servers).

  "
  (let*
      ((full-path      (expand-file-name path))
       (plain-name     (file-name-nondirectory path)))
    (if (not (string-equal plain-name "compile_commands.json"))
	(error "Unsupported file %s" plain-name))

    ;; Stash away the full path of  compile_commands.json
    ;; and create a server directory if needed.
    (setq _cow-rdm-compile-commands-json-path full-path)
    (make-directory (_cow-rdm-server-dir) t)

    ;; kill any previous cow-created RDM servers
    (message "TODO Find cow-created RDM servers")
    
    ;; Start rdm
    (message "TODO Find cow-created RDM servers")

    (message "done setting ")))


(defun _cow-rdm-processes ()
  "Currenly running RDM servers started by cow

Returns an alist with the process-attributes info with an added
'pid key."
  
  (remove-if-not
   #'(lambda (info)
       (and (not (string-match (regexp-quote (_cow-rdm-server-socket))
                               (cdr (assoc 'args info))))
            (string-equal "rdm" (cdr (assoc 'comm info)))))
   (mapcar #'(lambda (pid) (cons (cons 'pid pid)
                               (process-attributes pid)))
           (list-system-processes))))

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
  "Return the full path the directory which current RDM server is using"
  (concat _cow-rdm-dir-prefix 
	  (replace-regexp-in-string
	   "/" "_"
	   (expand-file-name (file-name-directory _cow-rdm-compile-commands-json-path)))))


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
