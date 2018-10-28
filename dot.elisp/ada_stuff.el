;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;---------------------------------------------------------
;;; include the directory $HOME/elisp in the load-path
;;;---------------------------------------------------------

(setq load-path (cons (expand-file-name "~/elisp") load-path))


;;;---------------------------------------------------------
;;; load Ada-Mode
;;;---------------------------------------------------------

(require 'ada-mode)
(require 'ada-stmt)  ;; use of statement templates
(require 'ada-xref)  ;; use of cross referencer

;;; make Emacs enter Ada-Mode when loading Ada source file
(setq auto-mode-alist (cons '("\\.ad[abs]$" . ada-mode)
                            auto-mode-alist))

;;; If you prefer to customize the Ada mode, comment out the needed
;;; line and set the value according to your system.

;(setq ada-spec-suffix ".ads")       ; Suffix of Ada specification files.
;(setq ada-body-suffix ".adb")       ; Suffix of Ada body files.
;(setq ada-language-version 'ada94)  ; Do we program in 'ada83 or 'ada94?

;(setq ada-indent 3)                 ; Defines the standard indentation. 
;(setq ada-broken-indent 2)          ; indent of  broken lines. 
;(setq ada-label-indent -4)          ; indent of a label.
;(setq ada-stmt-end-indent 0)        ; indent of statements end keywords in
                                     ; separate lines.  Examples are 'is',
                                     ; 'loop', 'record'.
;(setq ada-when-indent 3)            ; Defines the indentation for 'when'
                                     ; relative to 'exception' or 'case'. 
;(setq ada-indent-record-rel-type 3) ; Defines the indentation for 'record'
                                     ; relative to 'type' or 'use'. 
;(setq ada-indent-comment-as-code t) ; If non-nil, comment lines get indented
                                     ; as Ada code.
;(setq ada-indent-is-separate t)     ; If non-nil, 'is separate' or 'is
                                     ; abstract' on a separate line are
                                     ; indented. 
;(setq ada-indent-to-open-paren t)   ; If non-nil, following lines get indented
                                     ; according to the innermost open
                                     ; parenthesis.  

;(setq ada-auto-case t)              ; Non-nil automatically changes casing of
                                     ; preceeding word while typing.  In this
                                     ; case the following variables are
                                     ; evaluated.  They can be set to one of
                                     ; downcase-word, upcase-word,
                                     ; ada-loose-case-word or capitalize-word. 
;(setq ada-case-keyword 'downcase-word)          ; case of keywords.
;(setq ada-case-identifier 'ada-loose-case-word) ; case of identifiers.
;(setq ada-case-attribute 'upcase-word)          ; case of attributes.

;(setq ada-external-pretty-print-program "aimap") ; External pretty printer

;(setq ada-fill-comment-prefix "-- ")   ; Prefix for filling paragraphs of
                                        ; comments. 
;(setq ada-fill-comment-postfix " --")  ; Postfix for filling paragraphs.

;;; some less important variables
;(setq ada-tmp-directory "/tmp/")           ; Directory for temporary files.

;(setq ada-move-to-declaration nil)         ; If non-nil, ada-move-to-start
                                            ; moves point to the subprog
                                            ; declaration, not to 'begin'. 


; if the next variable is set to t the buffer is untabified and trailing
; spaces are removed before saving 

; (setq ada-clean-buffer-before-saving t)


;;;---------------------------------------------------------
;;; load and activate ada-format.el
;;; from Gary E. Barnes <geb@rational.com>
;;;---------------------------------------------------------

;(load "ada-format")
;(setq ada-tab-policy 'indent-af)


;;;---------------------------------------------------------
;;; autoload find-file.el and do settings for the use with Ada
;;;---------------------------------------------------------

(autoload 'ff-find-other-file "find-file" "Find this file's associated file." t)
(autoload 'ff-get-other-file "find-file"  nil t)
(make-variable-buffer-local 'ff-search-directories)
(make-variable-buffer-local 'ff-other-file-alist)

;;; you might consider setting these keys in the global-map, as it is
;;; very useful for C/C++ programming, too.
(define-key ada-mode-map "\C-co" 'ff-find-other-file)
(cond (window-system 
        (define-key ada-mode-map [C-mouse-2] 'ff-mouse-find-other-file)
	(define-key ada-mode-map [S-C-mouse-2]
	  'ff-mouse-find-other-file-other-window)))

(setq ada-search-directories
  '("." "/usr/adainclude" "/usr/local/adainclude"))

(setq ada-other-file-alist
  '(
    ("\\.ads$" (".adb")) ;; Ada specs and bodies
    ("\\.adb$" (".ads")) ;; GNAT filename conventions
    ))

(add-hook 'ada-mode-hook 
          '(lambda ()
             (setq ff-other-file-alist 'ada-other-file-alist)
             (setq ff-search-directories 'ada-search-directories)
	     (setq ff-pre-load-hooks 'ff-which-function-are-we-in)
	     (setq ff-post-load-hooks 'ff-set-point-accordingly)
	     (setq ff-file-created-hooks 'ada-make-body)
	     ))

;;;---------------------------------------------------------
;;; settings to use font-lock.el
;;;---------------------------------------------------------

(autoload 'font-lock-mode "font-lock"
  "Toggle Font Lock mode.
With arg, turn Font Lock mode on if and only if arg is positive." t)

(autoload 'turn-on-font-lock "font-lock"
  "Unconditionally turn on Font Lock mode." t)

;(autoload 'turn-on-lazy-lock "lazy-lock"
;  "Unconditionally turn on Lazy Lock mode.")

;(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)

(when (< 23 emacs-major-version)
  (cond (window-system (add-hook 'ada-mode-hook 'turn-on-font-lock)
                       (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
                       (add-hook 'c-mode-hook 'turn-on-font-lock)
                       (add-hook 'c++-mode-hook 'turn-on-font-lock)
                       (add-hook 'makefile-mode-hook 'turn-on-font-lock)
                       (add-hook 'compilation-mode-hook 'turn-on-font-lock)
                       
                       (add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
                       (add-hook 'BibTeX-mode-hook 'turn-on-font-lock)
                       (add-hook 'TeXinfo-mode-hook 'turn-on-font-lock)
                       (add-hook 'info-mode-hook 'turn-on-font-lock))))
  
;;;---------------------------------------------------------
;;; imenu.el
;;;---------------------------------------------------------

(autoload 'imenu-choose-buffer-index "imenu" "Menu of buffer index." t)
(autoload 'imenu "imenu" "Goto buffer index position." t)

(define-key global-map "\C-ci" 'imenu)
(cond (window-system 
        (define-key global-map [S-mouse-2] 'imenu)))


(add-hook 'ada-mode-hook
	  '(lambda ()
	     (setq imenu-create-index-function 'imenu-create-ada-index)))


;;;---------------------------------------------------------
;;; load package compile.el
;;;---------------------------------------------------------

(load "compile")


;;;---------------------------------------------------------
;;; end of dot.emacs
;;;---------------------------------------------------------


