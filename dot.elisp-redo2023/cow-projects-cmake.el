;;; -*- lexical-binding: t -*-

;;; Support for CMake projects for cow-load-project
;;;

(install-and-require-packages '(string-inflection cmake-mode))


(require 'cow-package-helpers)
(require 'cow-projects)
(require 'cow-cpp-setup)
(require 'system-cores)

(use-package company        :ensure t)
(use-package flycheck       :ensure t)
(use-package popup          :ensure t)


(defun _cow-predicate-cmake-proj (proj-file)
  (string-equal
   "CMakeLists.txt"
   (file-name-nondirectory proj-file)))

(defun _cow-setup-cmake-proj (proj-file)

  (let* ((src-dir               (file-name-directory proj-file))
         (build-dir             (_cow-cmake-build-dir proj-file))
         (compile_commands.json (cowguts-join-paths build-dir
                                                    "compile_commands.json"))
         (ninja                 (executable-find "ninja"))
         (job-count             (max 1 (- (system-cores :physical t) 1)))
         (generator             (if ninja "Ninja" "\"Unix Makefiles\"")))


    (if (file-regular-p compile_commands.json)
        (message "Found %s" compile_commands.json)
      (message "About to configure %s" build-dir)
      (_cow-configure-cmake-build-dir generator src-dir build-dir))
     
    (global-set-key [f9 ?f] '_cow-find-file-cmake)

    (cond
     ((equal cow-cpp-support 'use-rtags-cpp)
      ;; Fail if we can't find rdm
      (if (not (rtags-executable-find "rdm")) 
	  (error "Error: couldn't find rdm"))
      (cow-rdm-load-compile-commands-json compile_commands.json))
     ((equal cow-cpp-support 'use-lsp-cpp)
      nil))    
    
    ;; 
    ;; cowguts-register-project-type wants the setup function to return
    ;; A plist with the keys proj-file and compile-func.  So create and
    ;; return such a plist.
    ;;
    (setq compile-command (if ninja (format "%s -j %s -C \"%s\"" ninja job-count build-dir)
                            (format "make -j %s -C \"%s\"" job-count build-dir)))
    
    (list (cons 'proj-file    proj-file)
	  (cons 'compile-func #'(lambda ()
				  ;; let user modify compile command 
				  (let ((cmd (read-from-minibuffer
					      "Compile project command: "
					      compile-command nil nil
					      '(compile-history . 1))))
				    ;; Once user is happy, run the compile command. 
				    (compile cmd)))))))


(defun _cow-find-file-cmake () (interactive)
        (message "Sorry, find file for cmake isn't implemented yet."))


(cowguts-register-project-type '_cow-predicate-cmake-proj
                               '_cow-setup-cmake-proj)


(defun _cow-configure-cmake-build-dir (generator src-dir build-dir)
  "Configure the build directory."
  (message "(_cow-configure-cmake-build-dir %s %s %s)" generator src-dir build-dir)
  (save-excursion
    (let* ((buf (generate-new-buffer "*cow-cmake-populate-build-dir*"))
           (rc  (progn 
                  (switch-to-buffer buf)
                  (pop-to-buffer buf)
                  (call-process "cmake"
                                nil                                  ; infile
                                buf                                  ; dest
                                t                                    ; display
                                "-G" generator                       ; &rest
                                "-DCMAKE_BUILD_TYPE=Debug" 
                                "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                                "-S" src-dir 
                                "-B" build-dir))))
      (if (= rc 0)
          (bury-buffer buf)
        (message "Error: failed to configure build directory."))
      rc)))
      

(defun _cow-cmake-build-dir(&optional project-file)
  (message "inside (_cow-cmake-build-dir %s)"  project-file)
  (let* ((proj          (or project-file (cow-current-project-file)))
         (proj-dir      (file-name-directory proj))
         (base-proj-dir (car (last (seq-filter
                                    (lambda (x) (not (= (length x) 0)))
                                    (file-name-split proj-dir)))))
         (foo1          (message "cowguts-salt-licks-dir %s" (cowguts-salt-licks-dir)))
         (foo2          (message "proj %s" proj-dir))
         (foo3          (message "proj-dir %s" proj-dir))
         (foo4          (message "base-proj-dir %s" base-proj-dir))
         ;; (foo4          (message " %s" ))
         (scratch-dir   (cowguts-join-paths
                         (cowguts-salt-licks-dir)
                         (concat
                          "build-"
                          base-proj-dir
                          (replace-regexp-in-string "/" "_" proj-dir)))))
    (make-directory scratch-dir t)
    scratch-dir))


(provide 'cow-projects-cmake)
