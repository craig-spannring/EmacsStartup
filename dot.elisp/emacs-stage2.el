;;;
;;; Setup emacs
;;;
;;;
;;;
;;;
;;;
;;;


(setenv "PAGER" "cat")


(condition-case nil
    (system-name)
  (error
   (progn (defun system-name () (getenv "COMPUTERNAME")))))


(if (equal default-directory "/") 
    (setq default-directory "~/"))

(when (equal (system-name) "HYDRA") (setq default-directory "~/"))

;;; 
;;; Keep cygwin from complaining when emacs passes it 
;;; DOS style file names.
;;; 
(when (equal (system-name) "BZN00W145") 
  (when (not (getenv "CYGWIN"))
    (setenv "CYGWIN" "nodosfilewarning")))

(defun xemacs-p ()
  (and (boundp 'running-xemacs) running-xemacs))

(condition-case nil
    system-name
  (error
   (progn (setq system-name (system-name)))))

(require 'info)
(setq load-path (cons (expand-file-name "~/.elisp") load-path))
; (setq load-path (cons '"~/.elisp/elib-1.0" load-path))
(when (not (xemacs-p)) (setq load-path (cons '"~/.elisp/adamode-3.4a" load-path)))


; (message " cvs-update-flags %s"  cvs-update-flags)
; (sleep-for 10)

(cond ((not (and (boundp 'running-xemacs) running-xemacs))
       (load "setup_dired")))
(when (string= (system-name) "toledo.spannring.org")
  (progn 
    ;;(load "setup_vm")
    nil))
(when (featurep 'aquamacs)
  (load "setup_aquamacs"))
(load "setup_ws")
(load "latlong-utm")
(load "setup_gnats");
(load "setup_printer")
(load "setup_moon")
(load "setup_status_line")
(load "setup_latex")
(load "setup_tag_stuff")
(load "setup_global_keys")
(load "setup_other_global_settings")
(load "setup_stuff_for_cc_mode")
(load "setup_stuff_for_scala_mode")
(load "setup_stuff_for_ada_mode")
(load "setup_stuff_for_hilit")
(when (not (string= (system-name) "segovia.local")) (load "setup_timelog"))
; (load "setup_timeclock")
(load "setup_python")
(load "setup_haskell_mode")
(condition-case nil (load "setup_erlang.el") (error nil))
(condition-case nil (load "setup_msvc") (error nil))
(load "setup_pclcvs")
(load "setup_svn")
(load "gvd")
(load "setup_git")
(condition-case nil
  (load "setup_graphviz")
  (error nil))
;; (load "setup_doxystuff")
(load "setup_colors")
(load "qt_templates")
(load "setup_cedet")
(load "setup_imaxima")
(load "setup_mediawiki")
(load "fix-emacs24-drainbramage")

;; (when (not (xemacs-p)) (load "setup_w3"))

(setq w3-default-homepage "http://192.168.1.35")

(when (not (functionp 'cvs-examine))
  (load-library "pcl-cvs"))

(load "setup_weather");

; (load "setup_cscope")

(load "gpsim");

; (aset cvs-update-flags 1 (cons (cadr (aref cvs-update-flags 1))
;                               (cdr (aref cvs-update-flags 1))))




(cond ((not (string= (system-name) "HYDRA"))
       (condition-case nil
           (progn
             (cond
              ((not (string= system-type "windows-nt"))
               (server-start))
              (t
               (require 'gnuserv)
               (gnuserv-start))))
         (error nil))))


(cond ((string= system-type "emx")
       (setq os2help '("s:/emtex/book/english/latex.ndx"
                       "g:/ibmcpp/help/cpp.ndx"
;;                       "d:/ibmcpp/help/cppbrs.ndx"
))))

(cond ((eq window-system 'pm) (setq tex-dvi-view-command "vp")))

;; (when (not (xemacs-p))
;;   (cond 
;;    ((string= system-type "windows-nt")
;;     (require 'cygwin-mount)
;;     (cygwin-mount-activate)
;;     (setq Info-default-directory-list '("c:/cygwin/usr/local/embedded-arm-cross/info/dir"))
;;     (setq find-ls-option (quote ("-exec c:/cygwin/bin/ls.exe -ld {} ;" . "-ld"))))))


(cond ((string= (system-name) "BABBAGE")
       (defvar ispell-personal-dictionary "c:/usr/ispell/english")))

       
;;; The vc-annotate output was truncated without this setting. 
(when (or 
       (string= (system-name) "bznlinux021.spray.com")
       (string= (system-name) "bznlinux022.spray.com")
       (string= (system-name) "bznlinux006.spray.com"))
  (setenv "SVN_SSH" (expand-file-name "~/bin/svn_wrapper_for_emacs")))

(setenv "MANWIDTH" "80")
    
;(cond
; ((string= system-type 'darwin) (setenv "CVS_RSH" "$HOME/.bin/myssh1" nil t)))
(cond
 ((string= system-type 'darwin) (setenv "CVS_RSH" "ssh")))

(when nil
(custom-set-variables
 '(cvs-parse-ignored-messages (quote ("Executing ssh-askpass to query the password.*$" ".*Remote host denied X11 forwarding.*$" "cvs update: New directory .* -- ignored[ \\t]*$")))
 '(which-func-maxout 0)
 '(which-func-non-auto-modes nil)
 '(display-time-format nil)
 '(which-func-mode-global t nil (which-func))
 '(ada-label-indent 0)
 '(ada-clean-buffer-before-saving nil)

 '(cvs-mode-commit-hook (quote (cvs-insert-pr-summary)))

 '(cvs-edit-mode-hook (quote (cvs-insert-pr-summary)))
 '(cvs-edit-hook (quote (cvs-edit-insert-cvs-template cvs-edit-insert-changelog)))
 '(ada-search-directories (quote ("." "$ADA_INCLUDE_PATH"))))
)


(when (string= system-type "windows-nt")
  (setq Info-additional-directory-list (quote ("c:/cygwin/usr/local/embedded-arm-cross/info"))))
(when (string= system-name "segovia.local")
  (setq Info-additional-directory-list '("/Applications/Emacs.app/Contents/Resources/info/" "/usr/local/info/" "/usr/local/share/info/" "/usr/local/gnu/info/" "/usr/local/gnu/lib/info/" "/usr/local/gnu/lib/emacs/info/" "/usr/local/emacs/info/" "/usr/local/lib/info/" "/usr/local/lib/emacs/info/" "/usr/share/info/")))

(setq cc-other-file-alist (quote (("\\.cc$" (".hh" ".h")) ("\\.hh$" (".cc" ".C")) ("\\.c$" (".h")) ("\\.h$" (".cpp" ".c" ".cc" ".C" ".CC" ".cxx" ".cpp")) ("\\.C$" (".H" ".hh" ".h")) ("\\.H$" (".C" ".CC")) ("\\.CC$" (".HH" ".H" ".hh" ".h")) ("\\.HH$" (".CC")) ("\\.cxx$" (".hh" ".h")) ("\\.cpp$" (".h" ".hh" ".h")))))



(setq display-time-string-forms (quote ((format-time-string "%H:%M"))))
(setq display-time-mail-file 0)
(setq display-time-24hr-format t)

(custom-set-faces)
(when (not (xemacs-p)) (which-function-mode 1))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(when (and (string-equal system-name "cvs.montana.mid-tech.com")
           (string-equal system-type "berkeley-unix"))
  (setq msvc-ebrowse-exe "emacs-ebrowse"))

(when (or 
       (string-equal system-name "cvs.montana.mid-tech.com")
       (and (string-equal system-type "windows-nt")
            (string-equal system-name "HYDRA")))
  (load "my_ebrowse"))

(when (string= system-name "MARS") (setq msvc-hhc-path 
                                         "C:\\PROGRA~1\\WI2EF7~1\\4.20\\cepb\\help\\hhc.exe"))


(condition-case nil 
    (load "setup_jabber")
  (error 
   (message "jabber not found")))


