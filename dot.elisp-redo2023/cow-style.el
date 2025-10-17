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

(defconst _cow-coding-style-choices
  '(("COW"          .  "cow")
    ("User"         .  "user")
    ("CRG"          .  "crg")
    ("MI-STYLE"     .  "mi-style")
    ("NOT-MI-STYLE" .  "not-mi-style")
    ("PI-STYLE"     .  "pi-style")
    ("Gnu"          .  "gnu")
    ("K&R"          .  "k&r")
    ("BSD"          .  "bsd")
    ("Stroustrup"   .  "stroustrup")
    ("Whitesmith"   .  "whitesmith")
    ("Ellemtel"     .  "ellemtel")
    ("linux"        .  "linux")
    ("Python"       .  "python")
    ("Java"         .  "java")
    ("Awk"          .  "awk")))

(defun _cow-coding-style-create-type-choices ()
  ;; Build a `choice` type from the options
  `(choice ,@(mapcar (lambda (opt)
                       `(const :tag ,(car opt) ,(cdr opt)))
                     _cow-coding-style-choices)))


(defcustom cow-cc-coding-style (cdar _cow-coding-style-choices)
  "Which coding standard are we working with right now?"
  :type     (_cow-coding-style-create-type-choices)
  :options  (mapcar #'cdr _cow-coding-style-choices))
  
(defun cow-set-session-coding-style (style)
  "Set the coding style for this Emacs session to STYLE."
  (interactive
   (list (completing-read
          "Select coding style: "            ;; prompt
          (mapcar #'cdr
                  _cow-coding-style-choices) ;; collection
          nil                                ;; predicate
          t                                  ;; require-match
          nil                                ;; initial-input
          nil                                ;; hist
          cow-cc-coding-style)))             ;; default
  ;;  (setq cow-cc-coding-style style)
  ;;  (message "Coding style set to %s" cow-cc-coding-style))
  (c-set-style style)
  (setq cow-cc-coding-style style)
  (message  "Coding style set to %s" style))

(use-package clang-format+  :ensure t)
(use-package clang-format   :ensure t)
(clang-format--on-save-disable)

(provide 'cow-style)
