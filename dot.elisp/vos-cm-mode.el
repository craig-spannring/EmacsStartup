(defvar vos-cm-mode-syntax-table nil "")

(defvar vos-cm-mode-map nil "Keymap for Text mode.")

(defvar vos-cm-indent-level 3 "distance between tab stops")
 
(defun vos-cm-mode ()
  "Major mode for editing vos-cm intended for humans to read.
Special commands:
\\{vos-cm-mode-map}
Turning on Vos-Cm mode calls the value of the variable `vos-cm-mode-hook',
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map vos-cm-mode-map)
  (setq mode-name "vos-cm")
  (setq major-mode 'vos-cm-mode)
  (setq local-abbrev-table vos-cm-mode-abbrev-table)
  (setq tab-width vos-cm-indent-level)
  (run-hooks 'vos-cm-mode-hook))


(cond window-system 
      (hilit-set-mode-patterns 
       'vos-cm-mode 
       '(
         ("^ *& .*$" nil comment)
         ("^ *&$" nil comment)
         ("'[^'\n]*'" nil string)
         ("&[a-zA-z][a-zA-Z0-9_]*&" nil label)
         ("^&begin_parameters" "&end_parameters.*$" decl)
         ("&attach_input" nil keyword)
         ("&control" nil keyword)
         ("&detach_input" nil keyword)
         ("&display_line" nil keyword)
         ("&display_line_partial" nil keyword)
         ("&echo" nil keyword)
         ("&else" nil keyword)
         ("&eof" nil keyword)
         ("&eval" nil keyword)
         ("&goto" nil keyword)
         ("&if" nil keyword)
         ("&label" nil keyword)
         ("&mode" nil keyword)
         ("&return" nil keyword)
         ("&then" nil keyword)
         ("&set" nil keyword)
         ("&set_string" nil keyword))))
      
      