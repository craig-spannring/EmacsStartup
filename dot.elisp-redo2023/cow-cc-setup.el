;;; Sets of a few things for editing C/C++ code.
;;;


(message "Loading settings for cc-mode.")

(defcustom cow-cc-coding-standard "MI-STYLE"
  "Which coding standard are we working with right now?"
  :type '(string)
  :options '("PI-STYLE" "MI-STYLE", "stroustrup", "whitesmith", "linux"))

(mapc (lambda (style)  
        (if (not (boundp 'c-style-alist))
            (setq c-style-alist style)
          (setq c-style-alist 
                (append (list style)
                        c-style-alist))))
      '(("PI-STYLE"
         (c-basic-offset . 3)
         (c-offsets-alist . (
                             (substatement-open . 0)
                             (case-label . +)
                             (arglist-intro . +))))
        ("NOT-MI-STYLE"
         (c-basic-offset . 4)
         (c-comment-only-line-offset . 0)
         (c-offsets-alist
          (statement-block-intro . +)
          (knr-argdecl-intro . 0)
          (substatement-open . 0)
          (substatement-label . 0)
          (label . 0)
          (statement-cont . +)))       
        ("MI-STYLE" ;; Montana Instruments         
         (c-basic-offset . 4)
         (c-offsets-alist . ((inclass . ++)
                             (access-label . -)
                             (member-init-intro . 0)
                             (substatement-open . 0)
                             (case-label . +)
                             (inlambda . -)
                             (arglist-intro . +))))))

(add-hook 'c-mode-common-hook
          #'(lambda ()
              (message "setting C/C++ style to %s" cow-cc-coding-standard)
              (c-set-style cow-cc-coding-standard)))

(provide 'cow-cc-setup)
