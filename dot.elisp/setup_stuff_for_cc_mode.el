(when (not (xemacs-p)) (require 'find-file))

(when (string= (system-name) "segovia.local") 
  (setq software_version_string "")
  (setq company_name "Craig Spannring"))
(setq use_work_orders nil)

;; 
;; some settings for cc-mode
;;
(fmakunbound 'c-mode)
(makunbound 'c-mode-map)
(fmakunbound 'c++-mode)
(makunbound 'c++-mode-map)
(makunbound 'c-style-alist)

(load "cc-mode")

(when (not (xemacs-p))
  (show-paren-mode 1)
  (require 'find-file))

(define-key c-mode-map   "\C-co"           'msvc-find-other-file)
(define-key c-mode-map   (kbd "\C-x SPC")  'gud-break)
(define-key c++-mode-map "\C-co"           'msvc-find-other-file)
(define-key c++-mode-map [C-f12]           'hs-toggle-hiding)
(define-key c++-mode-map (kbd "\C-x SPC")  'gud-break)

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
  (require 'gud)
  (define-key c-mode-map   [S-right]   'gud-next)
  (define-key c-mode-map   [S-down]    'gud-step)
  (define-key c-mode-map   [S-up]      'gud-finish)
  
  (define-key c++-mode-map   [S-right]   'gud-next)
  (define-key c++-mode-map   [S-down]    'gud-step)
  (define-key c++-mode-map   [S-up]      'gud-finish)
  
  (define-key gud-mode-map   [S-right]   'gud-next)
  (define-key gud-mode-map   [S-down]    'gud-step)
  (define-key gud-mode-map   [S-up]      'gud-finish))

(setq auto-mode-alist
      (append '(("\\.C$"     . c++-mode)
                ("\\.cc$"    . c++-mode)
                ("\\.hh$"    . c++-mode)
                ("\\.c$"     . c-mode)
                ("\\.h$"     . c-mode)
                ("\\.m$"  . objc-mode)
                ) auto-mode-alist))

(setq c-basic-offset 3)
(setq c-echo-syntactic-information-p t)


(let ((pi-style '(("PI_STYLE"
                 (c-basic-offset . 3)
                 (c-offsets-alist . (
                                     (substatement-open . 0)
                                     (case-label . +)
                                     (arglist-intro . +))))))
      (midtech-style '(("MIDTECH_STYLE"
                 (c-special-indent-hook . (midtech_special-indentation))
                 (c-basic-offset . 2)
                 (c-offsets-alist . ((inclass . ++)
                                     (access-label . -)
                                     (member-init-intro . 0)
                                     (substatement-open . 0)
                                     (case-label . +)
                                     (arglist-intro . +)))))))
  (if (not (boundp 'c-style-alist))
      (setq c-style-alist pi-style)
    (setq c-style-alist 
          (append pi-style
                  c-style-alist)))
  (setq c-style-alist
        (append midtech-style
                c-style-alist)))


(defun linux-kernel-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(when (not (xemacs-p))
  (setq cc-other-file-alist 
        (append '(("\.cpp" (".h" ".hpp" ".hh"))) cc-other-file-alist))
  
  (setq cc-other-file-alist 
        (append '(("\.h" (".cpp" ".cxx" ".c"))) cc-other-file-alist))
  )


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
  (c-set-style "MIDTECH_STYLE")
  (setq comment_string_list 
        (midtech-change-h-file-comment-string comment_string_list))
  (setq tab-width 2)
  (when (not (string-match "XEmacs" emacs-version))
    (setq c-comment-continuation-stars " * "))
  (setq imenu-create-index-function 'imenu-tjcs-indexer)
  ;(c-set-style "PI_STYLE")

  (when (string= (system-name) "segovia.local")
    (local-set-key [f16] 'ebrowse-tags-find-definition)
    (local-set-key [S-f16] 'ebrowse-tags-find-declaration))
  (local-set-key "\r" 'newline-and-indent)
  ;; (turn-on-ws-trim)
)
(add-hook 'c-mode-common-hook 'my-cc-mode-common-hook)
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;
;;  put the icc c++ file extensions into the auto-mode-alist variable
;;  This lets Emacs give us the appropriate major mode for C++ files.
;;
(setq auto-mode-alist 
      (append auto-mode-alist 
              (list
               '("\\.sqx\\'" . c++-mode)
               '("\\.cpv\\'" . c++-mode)
               '("\\.hpv\\'" . c++-mode)
               '("\\.cxx\\'" . c++-mode)
               '("\\.hxx\\'" . c++-mode)
               '("\\.cpp\\'" . c++-mode) 
               '("\\.hpp\\'" . c++-mode))))
(setq auto-mode-alist 
      (cons '("\\.h\\'" . c++-mode) auto-mode-alist))

(setq auto-mode-alist 
      (cons '("\\.sqc\\'" . c-mode) auto-mode-alist))

;;
;; put the lex and yacc extensions into the auto-mode-alist variable so
;; Emacs can give us the proper major mode for yacc and flex files.
;;
(setq auto-mode-alist 
      (append 
       (list '("\\.y\\'" . c-mode) '("\\.l\\'" . c-mode))
       auto-mode-alist))

(cond ((string= (system-name) "babbage")
       (setq compile-command "nmake /nologo -f makefile.icc ")))

(require 'compile)

(setq compilation-error-regexp-alist
      (cons 
       '("\n\
\\([^( \t\n]+\\)[(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
:\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)
       compilation-error-regexp-alist))


;; This is the format of C assert failures on Ubuntu 8.04
;; (add-to-list 
;;  'compilation-error-regexp-alist
;;  '("\\([^\n:]\\)*: \\([^:]+\\):\\([0-9]+\\):.*Assertion" 2 3))

;; This is the format of C assert failures on Mac OS X with clang
(add-to-list 
  'compilation-error-regexp-alist
  '("^Assertion failed: .*, file \\([^,]+\\), line \\([0-9]+\\)\\.$" 1 2))


(load "c_functions")
(load "imenu_helpers")

;; Tell the c_functions routines to consider and '.hpv' file to be 
;; a C header file.
(setq header_file_extension_list (cons ".hpv" header_file_extension_list))

;; Set the comment string for Visual Builder user defined C++ source code.
(setq comment_string_list (cons '(".hpv" "// " "// " nil) comment_string_list))
(setq comment_string_list (cons '(".cpv" "// " "// " nil) comment_string_list))

;; Set the comment string for DB2 embeded SQL source file extensions.
(setq comment_string_list (cons '(".sqx" "// " "// " nil) comment_string_list))

(setq use_RCS_CRCs nil)


;; Setup stuff to help compilation mode handle cygwin paths on Windows
(when (string= system-type "windows-nt")
  (defun convert-cygwin-paths-to-win32 (name)
    "Convert a cygwin full path (e.g. /cygdrive/e/foo) into a win32 style name"
    (if (not (equal 0 (string-match "/cygdrive/[a-zA-Z]/" name)))
        name
      (format "%s:%s" (substring name 10 11) (substring name 11))))

  (assert (or (not (boundp 'compilation-parse-errors-filename-function))
              (not compilation-parse-errors-filename-function)
              (equal compilation-parse-errors-filename-function 
                     'convert-cygwin-paths-to-win32)))
  (setq compilation-parse-errors-filename-function 'convert-cygwin-paths-to-win32)
)


(condition-case nil 
  (load "csharp-mode")
  (error nil))
