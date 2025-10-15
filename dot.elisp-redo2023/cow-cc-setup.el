;;; Sets of a few things for editing C/C++ code.
;;;


(message "Loading settings for cc-mode.")


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

(c-add-style "MI-STYLE" ;; Montana Instruments         
             '((c-basic-offset . 4)
               (c-offsets-alist . ((inclass . ++)
                                   (access-label . -)
                                   (member-init-intro . 0)
                                   (substatement-open . 0)
                                   (case-label . +)
                                   (inlambda . -)
                                   (arglist-intro . +)))))


(defcustom cow-cc-coding-standard "whitesmith"
  "Which coding standard are we working with right now?"
  ;;                         cap     lower
  :type '(choice (const :tag "MI-STYLE"       "mi-style")
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
  :options '("mi-style"      ; These are offered as convenient choices in the Customize UI
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

(add-hook 'c-mode-common-hook
          #'(lambda ()
              (c-set-style cow-cc-coding-standard)))

(provide 'cow-cc-setup)
