;;; Setup coding style. 
;;;

(c-add-style "PI-STYLE"
             '((c-basic-offset . 3)
               (c-offsets-alist . (
                                   (substatement-open . 0)
                                   (case-label . +)
                                   (arglist-intro . +)))))
(c-add-style  "NOT-MI-STYLE"
              '((c-basic-offset . 4)
                (c-comment-only-line-offset . 0)
                (c-offsets-alist
                 (statement-block-intro . +)
                 (knr-argdecl-intro . 0)
                 (substatement-open . 0)
                 (substatement-label . 0)
                 (label . 0)
                 (statement-cont . +))))
(c-add-style "MI-STYLE"
             '((c-basic-offset . 4)
               (c-offsets-alist . ((inclass . ++)
                                   (access-label . -)
                                   (member-init-intro . 0)
                                   (substatement-open . 0)
                                   (case-label . +)
                                   (inlambda . -)
                                   (arglist-intro . +)))))
(c-add-style "cow"
             '((c-basic-offset . 2)
               (c-offsets-alist . ((inclass . ++)
                                   (access-label . -)
                                   (member-init-intro . 0)
                                   (substatement-open . 0)
                                   (case-label . +)
                                   (inlambda . -)
                                   (arglist-intro . +)))))
(c-add-style "crg"
  '("llvm"
    (c-basic-offset . 4) ; IndentWidth: 4
    (fill-column . 120)  ; ColumnLimit: 120
    (c-hanging-braces-alist . ((substatement-open after)
                               (brace-list-open)
                               (brace-entry-open)
                               (statement-case-open after)
                               (class-open after)
                               (defun-open after)
                               (inline-open after)))
    (c-hanging-colons-alist . ((case-label after)
                               (label after)
                               (access-label after)))
    (c-hanging-semi&comma-criteria . nil) ; Allow short statements on one line
    (c-cleanup-list . (brace-else-brace
                       brace-catch-brace
                       defun-close-semi
                       list-close-comma
                       scope-operator
                       empty-defun-braces
                       compact-empty-func-def))
    (c-offsets-alist . ((substatement-open . 0)
                        (statement-case-open . +)
                        (statement-cont . +)
                        (arglist-intro . +)
                        (arglist-close . 0)
                        (inline-open . 0)
                        (block-open . 0)
                        (brace-list-open . 0)))
    ))
(defcustom cow-cc-coding-standard "cow"
  "Which coding standard are we working with right now?"
  :type '(choice (const :tag "COW"            "cow")
                 (const :tag "User"           "user")
                 (const :tag "CRG"            "crg")
                 (const :tag "MI-STYLE"       "mi-style")
                 (const :tag "NOT-MI-STYLE"   "not-mi-style")
                 (const :tag "PI-STYLE"       "pi-style")
                 (const :tag "Gnu"            "gnu")
                 (const :tag "K&R"            "k&r")
                 (const :tag "BSD"            "bsd")
                 (const :tag "Stroustrup"     "stroustrup")
                 (const :tag "Whitesmith"     "whitesmith")
                 (const :tag "Ellemtel"       "ellemtel")
                 (const :tag "linux"          "linux")
                 (const :tag "Python"         "python")
                 (const :tag "Java"           "java")
                 (const :tag "Awk"            "awk"))          
  :options '("cow"     ; These are offered as convenient choices in the Customize UI
             "user"
             "crg"
             "mi-style" 
             "not-mi-style"  
             "pi-style"      
             "gnu"           
             "k&r"           
             "bsd"           
             "stroustrup"    
             "whitesmith"    
             "ellemtel"      
             "linux"         
             "python"        
             "java"          
             "awk"))


(use-package clang-format+  :ensure t)
(use-package clang-format   :ensure t)

(provide 'cow-style)
