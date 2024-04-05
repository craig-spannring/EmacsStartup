;;; -*- lexical-binding: t -*-

(require 'cow-package-helpers)
(require 'cow-projects)
(require 'cow-cpp-setup)

(use-package company        :ensure t)
(use-package flycheck       :ensure t)
(use-package popup          :ensure t)


(defun _cow-predicate-cmake-proj (proj-file)
  (string-equal
   "CMakeLists.txt"
   (file-name-nondirectory proj-file)))

(defun _cow-setup-cmake-proj (proj-file)

  (let* ((build-dir             (_cow-cmake-build-dir proj-file))
         (compile_commands.json (cowguts-join-paths build-dir
                                                    "compile_commands.json"))
         (ninja                 (executable-find "ninja"))
         (generator             (if ninja "-G Ninja" "\"Unix Makefiles\""))
         (cmake-cmd             (format "cmake %s -S \"%s\" -B \"%s\""
                                        generator
                                        (file-name-directory proj-file)
                                        build-dir)))
         
    (if (not (file-regular-p compile_commands.json))
          (compile cmake-cmd))
                  
    (global-set-key [f9 ?f] '_cow-find-file-cmake)

    
    ;; 
    ;; cowguts-register-project-type wants the setup function to return
    ;; A plist with the keys proj-file and compile-func.  So create and
    ;; return such a plist.
    ;;
    (setq compile-command (if ninja (format "%s -C \"%s\"" ninja build-dir)
                            (format "make -C \"%s\"" build-dir)))
    
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


(defun _cow-cmake-build-dir(&optional project-file)
  (message "inside (_cow-cmake-build-dir %s)"  project-file)
  (let* ((proj        (or project-file (cow-current-project-file)))
         (proj-dir    (file-name-directory proj))
         (foo         (message "proj-dir %s" proj-dir))         
         (scratch-dir (cowguts-join-paths
                       (cowguts-scratching-posts-dir)
                       (concat
                        "build-"
                        (replace-regexp-in-string "/" "_" proj-dir)))))
    (make-directory scratch-dir t)
    scratch-dir))


(provide 'cow-projects-cmake)
