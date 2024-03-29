
(install-and-require-packages '(modern-cpp-font-lock string-inflection))
(install-and-require-packages '(realgud-lldb))
(require 'find-file)


(setq use_work_orders nil)

; be-selective ;  ;; 
; be-selective ;  ;; some settings for cc-mode
; be-selective ;  ;;
; be-selective ;  (fmakunbound 'c-mode)
; be-selective ;  (makunbound 'c-mode-map)
; be-selective ;  (fmakunbound 'c++-mode)
; be-selective ;  (makunbound 'c++-mode-map)
; be-selective ;  (makunbound 'c-style-alist)
; be-selective ;
; be-selective ;  (load "cc-mode")
; be-selective ;
; be-selective ;  (when (not (xemacs-p))
(show-paren-mode 1)
; be-selective ;    (require 'find-file))
; be-selective ;
(define-key c-mode-map   "\C-co"    'msvc-find-other-file)
(define-key c++-mode-map "\C-co"    'msvc-find-other-file)
; be-selective ;  (define-key c++-mode-map [C-f12]    'hs-toggle-hiding)
; be-selective ;
(defconst setup_stuff_for_cc_mode-no-numeric-keypad-arrows
  (or (equal system-name "bznlinux035")
      (equal system-name "bznlinux035.spray.com"))
  "Is it likely that this system is missing the numeric keypad?

Systems such as 15inch laptops don't usually have a numeric keypad. 
We normally bind the kp-right, kp-down, and kp-up to gud-step,
gud-next, and gud-finish.  Without those gud-gdb is less unpleasant. 
We'll bind those functions to the shifted arrow keys to make gud-gdb 
more pleasant.")

(when setup_stuff_for_cc_mode-no-numeric-keypad-arrows
  (define-key c-mode-map   [S-right]   'gud-next)
  (define-key c-mode-map   [S-down]    'gud-step)
  (define-key c-mode-map   [S-up]   'gud-finish)
  
  (define-key c++-mode-map   [S-right]   'gud-next)
  (define-key c++-mode-map   [S-down]    'gud-step)
  (define-key c++-mode-map   [S-up]   'gud-finish)
  
; be-selective ;  (define-key gud-mode-map   [S-right]   'gud-next)
; be-selective ;  (define-key gud-mode-map   [S-down]    'gud-step)
; be-selective ;  (define-key gud-mode-map   [S-up]   'gud-finish)
  
  )

(add-to-list 'auto-mode-alist '("\\.C$"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c$"  . c-mode))
(add-to-list 'auto-mode-alist '("\\.h$"  . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$"  . objc-mode))

(setq c-basic-offset 3)
(setq c-echo-syntactic-information-p t)


(setq cts-additional-c-styles
      '(("PI_STYLE"
         (c-basic-offset . 3)
         (c-offsets-alist . (
                             (substatement-open . 0)
                             (case-label . +)
                             (arglist-intro . +))))
        ("MIDTECH_STYLE"
         (c-special-indent-hook . (midtech_special-indentation))
         (c-basic-offset . 2)
         (c-offsets-alist . ((inclass . ++)
                             (access-label . -)
                             (member-init-intro . 0)
                             (substatement-open . 0)
                             (case-label . +)
                             (arglist-intro . +))))
        ("NOT_MI_STYLE"
         (c-basic-offset . 4)
         (c-comment-only-line-offset . 0)
         (c-offsets-alist
          (statement-block-intro . +)
          (knr-argdecl-intro . 0)
          (substatement-open . 0)
          (substatement-label . 0)
          (label . 0)
          (statement-cont . +)))       
        ("MI_STYLE" ;; Montana Instruments         
         (c-special-indent-hook . (midtech_special-indentation))
         (c-basic-offset . 4)
         (c-offsets-alist . ((inclass . ++)
                             (access-label . -)
                             (member-init-intro . 0)
                             (substatement-open . 0)
                             (case-label . +)
                             (inlambda . -)
                             (arglist-intro . +))))))

(message "about to set c-style-alist")
(mapc (lambda (style)  
        (if (not (boundp 'c-style-alist))
            (setq c-style-alist style)
          (setq c-style-alist 
                (append (list style)
                        c-style-alist))))
      cts-additional-c-styles)


(defcustom current-coding-standard "MI_STYLE"
  "Which coding standard are we working with right now?"
  :type '(string)
  :options '("MIDTECH_STYLE" "PI_STYLE" "MI_STYLE"))
  ; (mapcar (lambda (style) (format "%s" (car style))) cts-additional-c-styles))

(defun linux-kernel-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))


; be-selective ;  (when (not (xemacs-p))
; be-selective ;     (setq cc-other-file-alist 
; be-selective ;           (append '(("\.cpp" (".h" ".hpp" ".hh"))) cc-other-file-alist))
; be-selective ;     
; be-selective ;     (setq cc-other-file-alist 
; be-selective ;           (append '(("\.h" (".cpp" ".cxx" ".c"))) cc-other-file-alist))
; be-selective ;     )

; be-selective ;
; be-selective ;
(defun midtech-change-h-file-comment-string (s)
  (cond
   ((not s) s)
   ((string-equal (caar s) ".h")
    (cons '(".h"  "// " "// " nil) 
          (midtech-change-h-file-comment-string (cdr s))))
   (t 
    (cons (car s) (midtech-change-h-file-comment-string (cdr s))))))

(defun my-cc-mode-common-hook ()
  (setq c-echo-syntactic-information-p t)
  (c-set-style current-coding-standard) ; "MIDTECH_STYLE")
  (setq comment_string_list 
        (midtech-change-h-file-comment-string comment_string_list))
  (setq tab-width 2)
  (when (not (string-match "XEmacs" emacs-version))
    (setq c-comment-continuation-stars " * "))
  (setq imenu-create-index-function 'imenu-tjcs-indexer)

  (when (string= (system-name) "segovia.local")
    (local-set-key [f16] 'ebrowse-tags-find-definition)
    (local-set-key [S-f16] 'ebrowse-tags-find-declaration))
  (local-set-key "\r" 'newline-and-indent)
  ;; (turn-on-ws-trim)
)
(add-hook 'c-mode-common-hook 'my-cc-mode-common-hook)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(add-hook 'c-mode-common-hook 'hs-minor-mode)

; be-selective ;
; be-selective ;  ;;
; be-selective ;  ;;  put the icc c++ file extensions into the auto-mode-alist variable
; be-selective ;  ;;  This lets Emacs give us the appropriate major mode for C++ files.
; be-selective ;  ;;
; be-selective ;  (setq auto-mode-alist 
; be-selective ;        (append auto-mode-alist 
; be-selective ;                (list
; be-selective ;                 '("\\.sqx\\'" . c++-mode)
; be-selective ;                 '("\\.cpv\\'" . c++-mode)
; be-selective ;                 '("\\.hpv\\'" . c++-mode)
; be-selective ;                 '("\\.cxx\\'" . c++-mode)
; be-selective ;                 '("\\.hxx\\'" . c++-mode)
; be-selective ;                 '("\\.cpp\\'" . c++-mode) 
; be-selective ;                 '("\\.hpp\\'" . c++-mode))))
; be-selective ;  (setq auto-mode-alist 
; be-selective ;        (cons '("\\.h\\'" . c++-mode) auto-mode-alist))
; be-selective ;
; be-selective ;  (setq auto-mode-alist 
; be-selective ;        (cons '("\\.sqc\\'" . c-mode) auto-mode-alist))
; be-selective ;
; be-selective ;  ;;
; be-selective ;  ;; put the lex and yacc extensions into the auto-mode-alist variable so
; be-selective ;  ;; Emacs can give us the proper major mode for yacc and flex files.
; be-selective ;  ;;
; be-selective ;  (setq auto-mode-alist 
; be-selective ;        (append 
; be-selective ;         (list '("\\.y\\'" . c-mode) '("\\.l\\'" . c-mode))
; be-selective ;         auto-mode-alist))
; be-selective ;
; be-selective ;  (cond ((string= (system-name) "babbage")
; be-selective ;         (setq compile-command "nmake /nologo -f makefile.icc ")))
; be-selective ;
(require 'compile)
; be-selective ;
; be-selective ;  (setq compilation-error-regexp-alist
; be-selective ;        (cons 
; be-selective ;         '("\n\
; be-selective ;  \\([^( \t\n]+\\)[(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
; be-selective ;  :\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)
; be-selective ;         compilation-error-regexp-alist))
; be-selective ;
; be-selective ;
; be-selective ;  ;; This is the format of C assert failures on Ubuntu 8.04
; be-selective ;  ;; (add-to-list 
; be-selective ;  ;;  'compilation-error-regexp-alist
; be-selective ;  ;;  '("\\([^\n:]\\)*: \\([^:]+\\):\\([0-9]+\\):.*Assertion" 2 3))
; be-selective ;
; be-selective ;  ;; This is the format of C assert failures on Mac OS X with clang
; be-selective ;  (add-to-list 
; be-selective ;    'compilation-error-regexp-alist
; be-selective ;    '("^Assertion failed: .*, file \\([^,]+\\), line \\([0-9]+\\)\\.$" 1 2))
; be-selective ;
; be-selective ;
(load "c_functions")
; be-selective ;  (load "imenu_helpers")
; be-selective ;
; be-selective ;  ;; Tell the c_functions routines to consider and '.hpv' file to be 
; be-selective ;  ;; a C header file.
; be-selective ;  (setq header_file_extension_list (cons ".hpv" header_file_extension_list))
; be-selective ;
; be-selective ;  ;; Set the comment string for Visual Builder user defined C++ source code.
; be-selective ;  (setq comment_string_list (cons '(".hpv" "// " "// " nil) comment_string_list))
; be-selective ;  (setq comment_string_list (cons '(".cpv" "// " "// " nil) comment_string_list))
; be-selective ;
; be-selective ;  ;; Set the comment string for DB2 embeded SQL source file extensions.
; be-selective ;  (setq comment_string_list (cons '(".sqx" "// " "// " nil) comment_string_list))
; be-selective ;
; be-selective ;  (setq use_RCS_CRCs nil)
; be-selective ;
; be-selective ;
; be-selective ;  ;; Setup stuff to help compilation mode handle cygwin paths on Windows
; be-selective ;  (when (string= system-type "windows-nt")
; be-selective ;    (defun convert-cygwin-paths-to-win32 (name)
; be-selective ;      "Convert a cygwin full path (e.g. /cygdrive/e/foo) into a win32 style name"
; be-selective ;      (if (not (equal 0 (string-match "/cygdrive/[a-zA-Z]/" name)))
; be-selective ;          name
; be-selective ;        (format "%s:%s" (substring name 10 11) (substring name 11))))
; be-selective ;
; be-selective ;    (assert (or (not (boundp 'compilation-parse-errors-filename-function))
; be-selective ;                (not compilation-parse-errors-filename-function)
; be-selective ;                (equal compilation-parse-errors-filename-function 
; be-selective ;                       'convert-cygwin-paths-to-win32)))
; be-selective ;    (setq compilation-parse-errors-filename-function 'convert-cygwin-paths-to-win32)
; be-selective ;  )
; be-selective ;
; be-selective ;
; be-selective ;  (condition-case nil 
; be-selective ;    (load "csharp-mode")
; be-selective ;    (error nil))
; be-selective ;
