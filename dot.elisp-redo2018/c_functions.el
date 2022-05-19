;; c_functions.el
;;
;; This collection of elisp functions will help to add comments
;; to C and C++ source files.
;; 
;; Functions:
;;    doxy-mark-code-region
;;       Add a @code tag to the beginning of the region, 
;;       an @endcode to the end of the region
;;    doxy-mark-verbatim-region
;;       Add a @verbatim tag to the beginning of the region, 
;;       an @endverbatim to the end of the region
;;    insert-c-file-header
;;       This function will place a standard file header comment
;;       at the top of the current file.
;;       The header comment will have a copyright notice with the 
;;       current year and the name of the source file.
;;       If the current file is a header file it will also
;;       place a '#ifndef THIS_FILE_h' line into the file
;;       to prevent this file from being included multiple times 
;;       if that line doesn't already exist.
;;    insert-c-rcsid
;;       Place rcs id lines in the source code.  They are 
;;       similar to the ones used by the VLT Inc. for the VLCS project.
;;       Very deprecated.  VLT doesn't exist anymore.  RCS shouldn't.
;;    add-c-function-tail-comment
;;       This function will figure out the name of the function 
;;       that starts on the current line and will place an
;;       ending comment right after the closing curly brace.
;;    insert-c-function-header
;;       This function will figure out the name of the function
;;       on the current (or succeeding) line and place a standard
;;       function header comment in front of the function.  The 
;;       header comment will contain the name of the function, 
;;       todays date, and the initials of the programmer.  
;;       (the initials are stored in variable 'name_of_coder')
;;    insert-c-class-header
;;       Same as 'insert-c-function-header' except that it
;;       works for C++ classes.
;;    insert-crev
;;       add a revision line to the function header comment
;;       before you call this function you must place the cursor
;;       on the line where the comment should be placed.
;;    insert-c-class
;;       Prompt the user for the name of the new class and it's 
;;       base class
;;       
;;  Customization variables:
;;    company_name
;;       string that hold the company name to place in copyright notices.
;;    use_work_order
;;       Determine whether function header comments should include
;;       work order numbers from the problem tracking system.
;;       if the value is nil it won't ask for a work order number
;;    name_of_coder
;;       string value that holds the initials of the individual 
;;       programmer.  This string is placed into some of the comments
;;    use_RCS_CRCs
;;       Determine whether the rcs revision string should include 
;;       the checksums for the source file.
;;    unique_inclusion_guards
;;       Use a unique id when creating header file multi-inclusion
;;       guard names 
;;    comment_string_list
;;
;;    software_version_string
;;    
;;    header_file_extension_list
;;
;;    comment_string_list
;;


;; beginning of user customizations
(defvar midtech-header-default-environment "OS Independent"
  "Default environment in the Mid-Tech file header comment")
(defvar midtech-header-default-compiler-type "Compiler Independent"
  "Default type of compiler in the Mid-Tech file header comment")

(defcustom company_name "We Write Code Inc., Bozeman, MT"
  "Name of the company for the copyright notice")
(defvar use_work_orders t
  "If non-nil then the function insert-c-function-header will prompt
for a work order number")
(defcustom name_of_coder "BugsBunny" 
  "Name of the programmer to put into function header comments")
(defcustom unique_inclusion_guards t 
  "Use a unique id when creating header file multi-inclusion
   guard names"
  :type '(boolean))
(defvar use_RCS_CRCs t
  "If this value is non-nil put a VLCS style checksum into the revision string.")
(defvar software_version_string (concat "$" "Header$")
  "Replace this string with some keyword used by your revision 
management system")
(defvar header_file_extension_list '(".h" ".hpp" ".H" ".HPP" ".ads")
  "List of all extension that should be considered to be header files")
(defvar csharp_file_extensions_list '(".cs")
  "List of all filename extensions that are considered to be C# files.")
(defvar ada_file_extensions_list '(".ada" ".adb" ".ads")
  "List of all filename extensions that are considered to be Ada files.")
(defvar comment_string_list '((".c"   "/* " " * " " */")
                              (".h"   "/* " " * " " */")
                              (".cpp" "// " "// " nil)
                              (".cxx" "// " "// " nil)
                              (".hpp" "// " "// " nil)
                              (".cs"  "// " "// " nil)
                              (".adb" "-- " "-- " nil) 
                              (".ads" "-- " "-- " nil))
  "List of file extensions and comment delimiters
This list is of the form 
  ((extension_string start_comment middle_comment end_comment) ...

Example of adding your own file extensions-
   (setq comment_string_list (cons '(\".hxx\" \"// \" \"// \" nil)")
(defcustom cts-c-file-header-style ""
  "What style of header comments?  Currently supported styles are
   MI_STYLE, MIDTECH_STYLE")
                              

;;
;; end of user customizations
;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

(require 'cl)

(defvar default_work_order ""
  "Last work order number given to insert-c-function-header")
(defvar StartOfComment  "/* " "")
(defvar MiddleOfComment " * " "")
(defvar EndOfComment    " */" "")



(defun is_spec_file (name)
  (is_extension_in_list name header_file_extension_list))

(defun is_csharp_file (name)
  (is_extension_in_list name csharp_file_extensions_list))

(defun is_ada_file (name)
  (is_extension_in_list name ada_file_extensions_list))

(defun is_extension_in_list (name l)
  (cond
   ((null l) nil)
   ((and 
     (> (length name) (length (car l))) ;; make sure 'name' is long enough
     (string= (car l) (substring name (- (length name) (length (car l))))))
    t)
   (t
    (is_extension_in_list name (cdr l)))))

(defun set-comment-constants ()
  "Determine what the comment characters should be for current file"
  (setq StartOfComment  "/* ") ;; set these just in case we don't later on
  (setq MiddleOfComment " * ")
  (setq EndOfComment    " */")

  (let (index ext)
    (setq index (- (length (buffer-file-name)) 1))
    (while (and (>= index 0)
                (not 
                 (equal "." (substring (buffer-file-name) index (+ 1 index)))))
      (setq index (- index 1)))
    (setq ext (substring (buffer-file-name) index))
    (set-comment-constants_R ext comment_string_list)))

(defun set-comment-constants_R (ext l)
  (cond
   ((null l) nil)
   ((string= ext (car (car l))) 
    (setq StartOfComment  (car (cdr (car l))))    ;; ((ext s m e) ...)
    (setq MiddleOfComment (car (cdr (cdr (car l)))))
    (setq EndOfComment    (car (cdr (cdr (cdr (car l)))))))
   (t
    (set-comment-constants_R ext (cdr l)))))

          
(defun read-work-order ()
  (let (result)
    (cond
     (use_work_orders
      (setq result (read-string (format "Work Order: (default '%s') " 
                                        default_work_order)))
      (cond 
       ((string= "" result)
        (setq result default_work_order)))
      (setq default_work_order result)
      )
     
     (t (setq result ""))
     )
    result)
  )

(defun Find_Extension (fname) 
  ;;
  ;; Make sure we have only a file name.  Otherwise the '.' character
  ;; we find might be from a directory name rather from the file name.
  ;;
  ;; TODO newer versions of elisp already have a function that does this. 
  ;; 
  (setq fname (Find_Simple_File_Name fname)) 
  (let (index)
    (setq index (- (length fname) 1))

    ;;
    ;; Now look backwards though the fname looking for the last '.'
    ;;
    (while (and (>= index 0) 
                (not 
                 (equal "." (substring fname index (+ index 1)))))
      (setq index (- index 1))
      )
    ;; index now points to the last '.' in 'fname'
    (cond
     ((> index 0) 
      (substring fname (+ 1 index)))
     (t
      ""))))
   

(defun Find_Simple_File_Name (fname) (file-name-nondirectory fname))


(defun figure_define_name (fname)
  "Determine the name we should use for multi-include protection 
   e.g. FOO_h_1567710eeb6bee29e6b3fdf5c77fc781
  "
  (let* ((basename (file-name-nondirectory fname))
         (name     (replace-regexp-in-string "\\." "_" 
                                             (file-name-sans-extension basename)
                                             t t))
         (suffix   (file-name-extension basename))
         (guard    (cond (unique_inclusion_guards 
                          (format "_%s"
                                  (md5 (format "%s%s%s"
                                               (current-time-string)
                                               name_of_coder
                                               fname
                                               (system-name)))))
                         (t ""))))
    (format "%s_%s%s" (upcase name) (downcase suffix) guard)))
                          

(defun Insert_Multi_Include_Protection_If_Needed (name)
  "Insert a '#ifdef this_file...' at the top of this 
file if it is a header file and doesn't already have one."
  
  (cond
   ((is_ada_file name) nil)
   (t
    (let (starting-pos define-name define-line)
      (setq starting-pos (point))
      (goto-char (point-min))
      
      (cond
       ((is_spec_file buffer-file-name)
        ;;
        ;; Now try to find a '#define THIS_FILE_h' line
        ;;
        (setq define-name (figure_define_name 
                           (expand-file-name (buffer-file-name))))
        (setq define-line 
              (format "^[ \t]*#[ \t]*define[ \t]+%s[ \t]*$" define-name))
        (goto-char (point-min))
        (cond 
         ((not (search-forward-regexp define-line (point-max) t))
          (goto-char (point-min))
          (insert (format "#ifndef %s\n" define-name))
          (insert (format "#define %s\n\n" define-name))
          (goto-char (point-max))
          (insert "\n\n#endif\n")))))
      (goto-char starting-pos)))))
   
    
(defun add-ada-fixhilit ()
  "Insert a function to work around the hilighting bugs in ada mode"
  (interactive)
  (beginning-of-line)
  (let ((start_point (point)))
    (insert "function fixhilit return integer is begin return 1; end;\n")
    (insert "pragma warnings(off, fixhilit);\n")
    (indent-region start_point (point) nil)
    (hilit-recenter nil)))

        
   
(defun insert-ada-file-header ()
  "Insert the file header comment"
  (interactive)
  (insert-c-file-header))

(defun _doxy-mark_region (start end prefix suffix)
  (message "start is %s, end is %s" start end)
  (save-excursion
    (goto-char end)
    (insert suffix)
    (insert "\n")

    (goto-char start)
    (insert "\n")
    (insert prefix)
    (insert "\n")))

(defun doxy-mark-verbatim-region (start end)
  "Enclose the current region inside of a @verbatim ... @endverbatim block"
  (interactive "r")
  (_doxy-mark_region start end "@verbatim" "@endverbatim")
)

(defun doxy-mark-code-region (start end)
  "Enclose the current region inside of a @code ... @endcode block"
  (interactive "r")
  (_doxy-mark_region start end "@code" "@endcode")
)

(defun insert-c-file-header ()
  "Insert the file header comment"
  (interactive)
  (set-comment-constants)
  (cond
   ((null (buffer-file-name))
    (message "Current buffer does not have a file name"))
   (t
    (let (simple-name index)
      (setq simple-name (Find_Simple_File_Name (buffer-file-name)))
      (Insert_Multi_Include_Protection_If_Needed 
       (Find_Simple_File_Name (buffer-file-name)))
      (goto-char (point-min))
      (cond
       ((equal cts-c-file-header-style "MI_STYLE")
        (mi_insert-header-comment-text simple-name))
       ((equal cts-c-file-header-style "MIDTECH_STYLE")
        (midtech_insert-header-comment-text simple-name))
       ((equal c-indentation-style "MIDTECH_STYLE")
        (midtech_insert-header-comment-text simple-name))
       (t (normal_insert-header-comment-text simple-name)))
      ))))



(defun normal_insert-header-comment-text (simple-name)
  (let (def-line)
    (insert (substring 
             (concat StartOfComment 
                     (substring "======================================"
                                1 (/ (- 75 (length simple-name)) 2))
                     " " 
                     simple-name
                     " " 
                     "=====================================================")
             0 78))
    (insert "\n")
    (insert MiddleOfComment)
    (insert "\n")
    (insert 
     (format "%s Copyright %04d by %s\n" MiddleOfComment
             (nth 2 (calendar-current-date)) company_name))
    (insert MiddleOfComment)
    (insert "\n")
    (insert MiddleOfComment)
    (insert " Def:   ")
    (setq def-line (point))
    (insert "\n")
    (insert MiddleOfComment)
    (insert "\n")
    (insert MiddleOfComment)
    (insert (concat "$" "Log$"))
    (insert "\n")
    (insert MiddleOfComment)
    (insert "\n")
    (insert MiddleOfComment)
    (insert "===========================================================================\n")
    (cond (EndOfComment (insert EndOfComment) (insert "\n")))
    (insert "\n")
    (goto-char def-line)
    )
  )


(defun midtech_insert-header-comment-text (simple-name)
  (let (def-line)
    (insert  "/**\n")
    (insert  " * @file\n")
    (insert  " * ")
    (setq def-line (point))
    (insert  "\n")
    (insert  " * \n")
    (insert  " * \n")
    (insert 
     (format " * Copyright %04d by %s\n"
             (nth 2 (calendar-current-date)) company_name))
    (insert  " * \n")
    (insert  " * @par Environment\n")
    (insert
     (format " * %s \n" midtech-header-default-environment))
    (insert  " * \n")
    (insert  " * @par Compiler\n")
    (insert
     (format " * %s \n" midtech-header-default-compiler-type))
    (insert  " * \n")
    ;; (insert 
    ;;  (format " * @author %s\n" name_of_coder))
    (insert  " *\n")
    (when (and software_version_string (not (string= "" software_version_string)))
      (insert 
       (concat " * " software_version_string) "\n"))
    (insert  " */    \n")
    (goto-char def-line)
    )
  )

(defun mi_insert-header-comment-text (simple-name)
  (let (def-line)
    (insert  "/**\n")
    (insert  " * @file\n")
    (insert  " * ")
    (setq def-line (point))
    (insert  "\n")
    (insert  " */    \n")
    (goto-char def-line)
    )
  )

(defun midtech-set-comment-defaults (type)
  "Set midtech-header-default-compiler-type midtech-header-default-environment vars

   These variables determine some of the text inserted by the
   insert-c-file-header function."
  
  (interactive (list (completing-read "Project Type: "
                                      '(("generic") 
                                        ("PlatformBuilder") 
                                        ("win32")
                                        ("evc4")
                                        ("msvc6")
                                        ("gcc-centerline")
                                        ("uvision")
                                        ("blackmore")
                                        ("rowbot"))
                                      nil
                                      t)))
  (cond
   ((string= (downcase type) "generic")
    (setq midtech-header-default-compiler-type "Compiler Independent")
    (setq midtech-header-default-environment   "OS Independent"))
   ((string= (downcase type) "platformbuilder")
    (setq midtech-header-default-compiler-type "Platform Builder")
    (setq midtech-header-default-environment   "WinCE Platform"))
   ((string= (downcase type) "win32")
    (setq midtech-header-default-compiler-type "MSVC++ 6.0, eVC 3.0, eVC 4.0")
    (setq midtech-header-default-environment   "Win32, WinCE 2.12, WinCE .NET 4.2"))
   ((string= (downcase type) "evc4")
    (setq midtech-header-default-compiler-type "eVC 4.0")
    (setq midtech-header-default-environment   "WinCE .NET 4.0"))
   ((string= (downcase type) "msvc6")
    (setq midtech-header-default-compiler-type "MSVC++ 6.0")
    (setq midtech-header-default-environment   "Win32"))
   ((string= (downcase type) "rowbot")
    (setq midtech-header-default-compiler-type "IAR")
    (setq midtech-header-default-environment   "STR712 Board with librowbot"))
   ((string= (downcase type) "uvision")
    (setq midtech-header-default-compiler-type "Kiel")
    (setq midtech-header-default-environment   "LPCFamily libplatform"))
   ((string= (downcase type) "blackmore")
    (setq midtech-header-default-compiler-type "gcc")
    (setq midtech-header-default-environment   "Linux with libblackmore"))
   ((string= (downcase type) "gcc-centerline")
    (setq midtech-header-default-compiler-type "gcc 3.1")
    (setq midtech-header-default-environment   "SA1100 Board for CenterLine"))))
                                      

  
(defun cvt_file_name_to_rcsid_var_R (n i)
  (cond
   ((= (- i (length n)) 0) "")
   ((string= (substring n i (+ 1 i)) ".") 
    (concat "_" (cvt_file_name_to_rcsid_var_R n (+ 1 i))))
   (t
    (concat (substring n i (+ 1 i)) 
            (cvt_file_name_to_rcsid_var_R n (+ 1 i))))))

(defun cvt_file_name_to_rcsid_var (fname all_caps)
  (cond 
   (all_caps (upcase (cvt_file_name_to_rcsid_var_R fname 0)))
   (t        (cvt_file_name_to_rcsid_var_R fname 0))))

(defun insert-ada-rcsid ()
  "insert rcsid lines"
  (interactive)
  (cond 
   ((null (buffer-file-name))
    (message "Current buffer does not have a file name"))
   (t
    (beginning-of-line)
    (let ((start_point (point))
          (var_name    (cond 
                        ((is_spec_file buffer-file-name) "rcs_spec_revision_id")
                        (t                               "rcs_body_revision_id"))))
      (insert (format "   %s: constant string := \"%s"
                      var_name software_version_string))
      (cond (use_RCS_CRCs
             (insert (format " RCSREV_AUX ( \"\" , %s_crc , \"00000000\" )"
                             (cvt_file_name_to_rcsid_var simple-name t)))))
      (insert (format "\";\n"))
      (insert (format "pragma warnings(off, %s);\n" var_name))
      (indent-region start_point (point) nil)
      )
    )
   )
  )


(defun insert-c-rcsid ()
  "Insert rcsid lines.  Needless to say, this is old and deprecated."
  (interactive)
  (cond
   ((null (buffer-file-name))
    (message "Current buffer does not have a file name"))
   (t
    (let (def-line simple-name index)
      (setq simple-name (Find_Simple_File_Name (buffer-file-name)))
      
      (cond 
       ((is_spec_file buffer-file-name)
        (insert 
         (format "static char  rcsid_%s [ ] =\n" 
                 (cvt_file_name_to_rcsid_var simple-name nil)))
        (cond (use_RCS_CRCs
               (insert (format "         \"%sId$\"" "$"))
               (insert (format " RCSREV_AUX ( \"\" , %s_crc , \"00000000\" )"
                               (cvt_file_name_to_rcsid_var simple-name t))))
              (t
               (insert (format "         \"%s\"" software_version_string))))
        (insert ";\n")
        (insert (format "static void *no_unused_%s_warn[]={rcsid_%s, "
                        (cvt_file_name_to_rcsid_var simple-name nil)
                        (cvt_file_name_to_rcsid_var simple-name nil)))
        (insert (format "no_unused_%s_warn};\n" 
                        (cvt_file_name_to_rcsid_var simple-name nil))))
       (t 
        (insert (format "static char  software_version[]   = \"%s\";\n"
                        software_version_string))
        (cond (use_RCS_CRCs
               (insert "static char  rcsid[] =\n")
               (insert (format "         \"%sId$\"" "$"))
               (insert (format " RCSREV_STR ( \"\" , %s_crc , \"00000000\" )"
                               (cvt_file_name_to_rcsid_var simple-name t)))
               (insert  ";\n")))
        (insert "static void *no_unused_var_warn[] = {software_version,\n")
        (cond (use_RCS_CRCs
               (insert "                                     rcsid,\n")))
        (insert "                                     no_unused_var_warn};\n")))))))

(defun add-c-function-tail-comment (func-name)
  "Place a comment after the closing brace for current function"
                         ; 
  (interactive (list (read-string 
                      (format "Function Name: (default '%s') " 
                              (figure-possible-c-function-name t)))))


  (setq func-name 
        (cond 
         ((string= "" func-name) (figure-possible-c-function-name t))
         (t                      func-name)))


  (let (starting-pos)
    (setq starting-pos (point))
    (set-comment-constants)
    (beginning-of-line)
    (cond
     ((search-forward "{")
      (backward-char 1)
      (forward-list 1)
      (insert " ")
      (insert StartOfComment)
      (cond ((equal c-indentation-style "MIDTECH_STYLE")(insert "End ")))
      (insert func-name) 
      (cond ((not (equal c-indentation-style "MIDTECH_STYLE"))(insert "() ")))
      (cond (EndOfComment (insert EndOfComment)))))
    (goto-char starting-pos)))


(defun figure-possible-c-class-name ()
  (let (starting-pos start-of-line start-of-fname end-of-name result)
    (setq starting-pos (point))
    (beginning-of-line)
    (setq start-of-line (point))
    (cond 
     ((search-forward "class")
      (forward-sexp)
      (setq end-of-name (point))
      (backward-sexp)
      (setq result (buffer-substring (point) end-of-name)))
     (t (setq result "")))
    (message "%s" result)
    (goto-char starting-pos)
    result))

(defun backup_over_class_specifiers ()
  (cond ((and (> (- (point) (point-min)) 2) 
              (string-equal "::" (buffer-substring (- (point) 2) (- (point) 0))))
         (backward-char 2)
         (backward-sexp)
         (backup_over_class_specifiers))))

(defun figure-possible-c-function-name (with_class)
  (let (starting-pos start-of-line start-of-fname end-of-fname result)
    (setq starting-pos (point))
    (beginning-of-line)
    (setq start-of-line (point))
    (cond 
     ((search-forward "(")  ; search for the end of the name
      (goto-char (- (point) 1))
      ;; should be near the last character of the function name
      (backward-sexp)
      (forward-sexp)
      (setq end-of-fname (point))
      (backward-sexp)
      ;; we are on the first character of the function name
      (cond
       ;; optionally move back to the beginning of the class name
       (with_class (backup_over_class_specifiers)))
      ;; If we are paying attention to classes we are now on
      ;; the first character of the first class name
      (setq start-of-fname (point))
      (cond
       ((>= (point) start-of-line)
        (setq result (buffer-substring end-of-fname  start-of-fname)))
       (t    (setq result ""))))
     (t
      (setq result "")))
    (message "%s" result)
    (goto-char starting-pos)
    result))
  

(defun insert-c-function-header (func-name work-order)
  "Insert a function header comment template before the current point"
                                        ;  (interactive "*sFunction Name: 
                                        ; sWork Order: ")
  (interactive (list (read-string 
                      (format "Function Name: (default '%s') " 
                              (figure-possible-c-function-name 
                               (equal "MIDTECH_STYLE" c-indentation-style))))
                     (read-work-order)))
(message (format "The function name is |%s|" func-name))
  (insert-c-function-or-class-header func-name work-order "()" t nil))
  
(defconst insert-c-class-hist-list '("" "public" "protected" "private"))

(defun insert-c-class (class-name access)
  "Insert a class template before the current point"
  
  (interactive (list (read-string (format "Class Name: "))
                     (completing-read "Base Access: " 
                                      '(("") ("public") ("protected") ("private"))
                                      nil
                                      t
                                      ""
                                      (cons 'insert-c-class-hist-list 0))))
  (let (start-pos active-line end-pos (derivation ""))

    (cond 
     ((not (string= access ""))
      (setq base (read-string "Base Class: "))
      (setq derivation (format ": %s %s" access base))))

    (setq start-pos (point))

    (insert " ")

    (insert "/** \n")
    (setq active-line (point))
    (insert " *  \n")
    (insert " * \n")
    (insert " * \n")
    (insert " * \n")
    (insert " */ \n")
    (insert (format "class %s %s\n" class-name derivation))

    (insert "{ \n")
    (insert "  // Enums, Structures, etc. \n")
    (insert "  public: \n")
    (insert "  protected: \n")
    (insert "  private: \n")
    (insert " \n")
    (insert "  // Methods \n")
    (insert "  public: \n")
    (insert "  protected: \n")
    (insert "  private: \n")
    (insert " \n")
    (insert "  // Data Members \n")
    (insert "  public: \n")
    (insert "  protected: \n")
    (insert "  private: \n")
    (insert " \n")
    (insert "}; \n")
    (setq end-pos (point))
    (goto-char active-line)
    (end-of-line)
    (indent-region start-pos end-pos nil)
    ))
    
                

(defun insert-c-class-header (func-name work-order)
  "Insert a function header comment template before the current point"
                                        ;  (interactive "*sFunction Name: 
                                        ; sWork Order: ")
  (interactive (list (read-string 
                      (format "Class Name: (default '%s') " 
                              (figure-possible-c-class-name)))
                     (read-work-order)))
  (insert-c-function-or-class-header func-name work-order "::" nil t)
  )
         
(defun insert-c-function-or-class-header (func-name work-order suffix is_func is_class)
  "Insert a function header comment template before the current point"

(message (format "2The function name is |%s|" func-name))

  (set-comment-constants)
  (setq func-name 
        (cond 
         ((string= "" func-name) (cond (is_class (figure-possible-c-class-name))
                                       (t (figure-possible-c-function-name 
                                           (equal "MIDTECH_STYLE" c-indentation-style)))))
         (t                      func-name)))

  (beginning-of-line)
  
  (goto-char 
   (cond
    ((string= c-indentation-style "MIDTECH_STYLE")
     (midtech_insert-c-function-or-class-header func-name work-order suffix is_func is_class))
    (t
     (normal_insert-c-function-or-class-header func-name work-order suffix is_func is_class)))     
   )
  )


(defun normal_insert-c-function-or-class-header (func-name work-order suffix is_func is_class)
  (let (def-line)
    (insert (substring 
             (concat StartOfComment 
                     (substring "======================================"
                                1 (/ (- (- 75 (length suffix)) (length func-name)) 2))
                     " " 
                     func-name
                     suffix " " 
                     "======================================================")
             0 78))
    (insert "\n")
    (insert MiddleOfComment)
    (insert "\n")
    (insert MiddleOfComment)
    (insert "Def:  ")
    (setq def-line (point))
    (insert "\n")
    (insert MiddleOfComment)
    (insert "\n")
    (insert MiddleOfComment)
    (cond
     (is_func     
      (insert "Ret:  \n")
      (insert MiddleOfComment)))
    (insert "\n")
    (insert MiddleOfComment)
    (insert "Rev:\n")
    (insert MiddleOfComment)
    (insert "   $Crev$   " 
            (format "%4d/%02d/%02d  %s  %s\n" (nth 2 (calendar-current-date))
                    (nth 0 (calendar-current-date)) 
                    (nth 1 (calendar-current-date))
                    work-order  name_of_coder))
    (insert MiddleOfComment)
    (insert "      Initially coded.  \n")
    (insert MiddleOfComment)
    (insert "\n")
    (insert MiddleOfComment)
    (insert "===========================================================================\n")
    (cond (EndOfComment (insert " */\n")))
    def-line)
  )


(defun midtech_insert-c-function-or-class-header (func-name work-order suffix is_func is_class)
  (let (def-line)
    (insert "/**\n")
    (insert " * ")
    (setq def-line (point))
    (insert "\n")
    (insert " *\n")
    (insert " * \n")
    (insert " *\n")
    (insert " * @param  \n")
    (insert " * @return  \n")
    (insert " *\n")
    (insert " * @exception \n")
    (insert " */ \n")
    def-line)
  )

(defun insert-crev (work-order)
  "Insert a crev comment header before the current point"
  (interactive (list (read-work-order)))
  
  (beginning-of-line)
  (let (comment-line)
    (insert MiddleOfComment)
    (insert "    $Crev$   " 
            (format "%4d/%02d/%02d  %s  %s\n" (nth 2 (calendar-current-date))
                    (nth 0 (calendar-current-date)) 
                    (nth 1 (calendar-current-date))
                    work-order  name_of_coder))
    (insert MiddleOfComment)
    (insert "       ")
    (setq comment-line (point)) 
    (insert "\n")
    (goto-char comment-line)
    )
  )


(defun midtech_special-indentation-current-line ()
  (save-excursion
    (let ((orig (point)) start end)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq end (point))
;;      (message "%d %d %d %s" orig start end (buffer-substring start end))
      (buffer-substring start end))))


(defun midtech_move-to-next-non-blank-line ()
  (let ((get-syntax '(lambda (x) (if (atom x) x (car x)))))
    (while (and (or
                 (member 'comment-intro 
                         (mapcar get-syntax (c-guess-basic-syntax)))
                 (progn                   
                   ;; skip until we get to the next non-blank line
                   (beginning-of-line)
                   (re-search-forward
                    "^[ \t]*$" (line-end-position) t)))
                (not (equal (point) (point-max))))
      (forward-line))))

(defun midtech_special-indentation ()
  "Special indentation to meet requirements of TeeJet coding standard."
  (save-excursion
    (let ((need-to-unindent t)
          (starting-point (point))
          (starting-line (save-excursion (beginning-of-line) (point)))
          get-current-line 
          (get-syntax '(lambda (x) (if (atom x) x (car x))))
          (syntax-list (mapcar '(lambda (x) (if (atom x) x (car x)))
                               (c-guess-basic-syntax))))
      (cond 
       ((and (member 'inclass syntax-list)
             (or 
              (member 'inline-open syntax-list)
              (midtech-cc-mode-inside-of-struct-p)))
        (setq need-to-unindent t))
       ((and (member 'comment-intro syntax-list)
             (member 'inclass syntax-list))
        (end-of-line)

        ;; Try to move the point to the next non-blank, non-comment line
        (midtech_move-to-next-non-blank-line)

        (setq need-to-unindent 
              (cond 
               ((equal (point) (point-max))
                ;; There was nothing interesting past the comment
                (goto-char starting-point)
                
                ;; Move the point to the first line of this class
                (goto-char (midtech-cc-mode-start-bracket-of-class))
                
                ;;(message "nothing of interest (%s)" (point))
                (forward-line)
                (midtech_move-to-next-non-blank-line)
                ;;(message "the point is %s" (point))
                (>= (point) starting-line)
                nil)
               (t
                (member 'access-label 
                        (mapcar get-syntax 
                                (c-guess-basic-syntax)))))))
       ((and (not (string-match "XEmacs" emacs-version))
             (member 'access-label syntax-list)
             (member 'inclass syntax-list))
        ;; We have to take care of public/protected/private 
        ;; and posibly unindent comments before it in the class
        (let ((old-value c-echo-syntactic-information-p)
              (start-of-class (midtech-cc-mode-start-bracket-of-class)))
          (setq c-echo-syntactic-information-p nil)
          (forward-line -1)
          (while (and (> (point) (point-min))
                      (> (point) start-of-class)
                      (or nil
                          (member 'comment-intro
                                  (mapcar get-syntax 
                                          (c-guess-basic-syntax)))))
            (indent-according-to-mode)
            (forward-line -1)
            (beginning-of-line))
          (setq c-echo-syntactic-information-p old-value)
          (setq need-to-unindent nil)))
       (t (setq need-to-unindent nil)))
      
      (cond 
       (need-to-unindent 
        ;;(message "we're here")
        (goto-char starting-line)
        (cond
         ((equal (format (format "%%%ds" c-basic-offset) "")
                 (buffer-substring (point) (+ (point) c-basic-offset)))
          ;;(message "deleting stuff")
          (delete-char c-basic-offset))))))))


(defun midtech-cc-mode-start-bracket-of-class()
  "Return the position of the opening bracket for the class that 
point is inside of"
  (let ((tmp (c-guess-basic-syntax)))
    (while (and tmp (not (equal 'inclass (caar tmp))))
      (setq tmp (cdr tmp))
      ;;(message "tmp is %s" tmp)
      )
    (midtech-get-pos-c-guess tmp)))

(defun midtech-cc-mode-inside-of-struct-p()
  "Return t if the point is actually inside of a 
structure instead of a class"
  (save-excursion
    (let ((bracket (midtech-cc-mode-start-bracket-of-class)))
      (cond 
       (bracket 
        (goto-char bracket)
        (setq start (midtech-get-pos-c-guess (c-guess-basic-syntax)))
        (and start (string= (buffer-substring start (+ start 6)) "struct")))
       (t nil)))))  

;; (defun midtech-cc-mode-inside-of-struct-p()
;;   "Return t if the point is actually inside of a 
;; structure instead of a class"
;;   (save-excursion
;;     (let ((bracket (midtech-cc-mode-start-bracket-of-class)))
;;       (cond 
;;        (bracket 
;;         (goto-char (midtech-fix-guess bracket))
;;         (setq start (midtech-fix-guess (cdar (c-guess-basic-syntax))))
;;         (and start (string= (buffer-substring start (+ start 6)) "struct")))
;;        (t nil)))))  


(defun midtech-get-pos-c-guess (tmp)
  (let ((tmp (cdar tmp)))
    (message "tmp is %s" tmp)
    (if (listp tmp)
        (car tmp)
      tmp)))
    
    
(defun midtech-fix-guess (tmp)
  (if (and (listp tmp) (= (length tmp) 1))
      (car tmp)
    tmp))

(defun midtech-cleanup-guess-results (tmp)
  (setq tmp (if (listp (cdar tmp))
                (car (cdar tmp))
              (cdar tmp)))
  (message "returning %s " tmp)
  tmp)
  
(defun midtech-grab-comment-words (section end-section)
  (let ((tmp nil) (result nil) (str "") (start 0))
    (save-excursion
      (while (<= section end-section)
        (goto-char section)
        (setq char (buffer-substring-no-properties section (+ 1 section)))
        (setq tmp (cons (cons char (midtech-current-column)) tmp))
        (setq section (+ 1 section))
        )
      )
    (setq tmp (reverse tmp))
    (while tmp
      (setq start (cdar tmp))
      (setq str "")
      ;; Get the run of like-characters 
      (cond 
       ((or (string-equal (caar tmp) " ") (string-equal (caar tmp) "\t"))
            (while (and tmp (or (string-equal (caar tmp) " ") (string-equal (caar tmp) "\t")))
              (setq str (concat str (caar tmp)))
              (setq tmp (cdr tmp))
              )
            )
        
       ((string-equal (caar tmp) "\n")
         (setq str "\n")
         (setq tmp (cdr tmp)))

       ((string-equal (caar tmp) "\r")
        (cond 
         ((and (cdr tmp) (string-equal "\n" (caadr tmp)))
          (setq str "\r\n")
          (setq tmp (cddr tmp)))
         (t
          (setq str "\r")
          (setq tmp (cdr tmp)))))

       (t
        (while (and tmp (not (or (string-equal (caar tmp) " ")  (string-equal (caar tmp) "\t")
                                 (string-equal (caar tmp) "\n") (string-equal (caar tmp) "\r")
                    )))
          (setq str (concat str (caar tmp)))
          (setq tmp (cdr tmp))
          )
        )
       )

      (setq result (cons (cons str start) result))
      )
    (reverse result)
    )
  )
      
(defun midtech-add-parameter-to-list (params name description mode)
  ;; First remove the trainling line ending from the description
  (cond 
   ((and (>= (length description) 2) 
         (string= "\r\n" (substring description (- (length description) 2))))
    (setq description (substring description 0 (- (length description) 2))))
   ((and (>= (length description) 1) 
         (or 
          (string= "\n" (substring description (- (length description) 1)))
          (string= "\r" (substring description (- (length description) 1)))))
    (setq description (substring description 0 (- (length description) 1)))))
  ;; (message "(midtech-add-parameter-to-list |%s| |%s| |%s| |%s|)" params name description mode)
  (let (result)
    (cond 
     ((and (or (string= name "None") (string= name "none"))
           (or (string= description " * ") 
               (string= description "\n * ")
               (string= description "\r\n * ")
               (string= description "") 
               (string= description "\n")
               (string= description "\r\n")))
      params)
     (t
      (cond
       ((null params)
        (setq result (list (list name description mode)))
        result)
       
       ((string= name (caar params))
        (cond 
         ((string= description (second (car params)))
          nil)
         (t 
          (setq description (concat description "\n" (second (car params))))))
        (cons (list name description (concat (third (car params)) "/" mode)) (cdr params)))
       (t
        (cons (car params) (midtech-add-parameter-to-list (cdr params) name description mode))))
      ))))
     
     

(defun midtech-find-parameter-description
  (start 
   end
   mode
   params
   SectionStartPattern 
   SectionEndPattern)
;;  "This should return a list of the parameters and their descriptions.
;;The list is of the form 
;;   ((name1 description1 mode1) (name2 description2 mode2) ...)
;;"
  (let (foo end-section section column word-end word-start found tmp
            characters
            pos (name "") (description ""))
    
    ;; Try to find the parameters
    (goto-char start)
    (setq section (re-search-forward SectionStartPattern end))
    (setq end-section 
          (progn 
            (re-search-forward SectionEndPattern end)
            (previous-line 1)
            (line-end-position)))
    (goto-char section)
    (setq column (midtech-current-column))
    
    (setq words  (midtech-grab-comment-words section end-section))

    ;; Now remove anything that is before the appropriate column
    (setq tmp nil)
    (while words 
      (cond
       ((>= (cdar words) column)
        (setq tmp (cons (car words) tmp))))
      (setq words (cdr words)))
    (setq words (reverse tmp))


    (setq name nil)
    (setq description "")
    (while words 
      (cond
       ((= column (cdr (car words)))
        (cond
         (name 
          (setq params (midtech-add-parameter-to-list params name description mode))))
        (setq description "")
        (setq name (caar words)))
       (t
        (setq description (concat description (caar words)))
        (cond ((and 
                (not (and (cdr words) (= column (cdr (car (cdr words))))))
                (or (string= "\n" (caar words))
                    (string= "\r" (caar words))
                    (string= "\r\n" (caar words))))
               (setq description (concat description " * "))
                ))
))
      (setq words (cdr words))
      )
    (cond 
     (name 
      (setq params (midtech-add-parameter-to-list params name description mode))))
    )
  params
)

  
(defun midtech-comment-words-after-column (column section end-section)
  (let (tmp words (result ""))
      (setq words (midtech-grab-comment-words section end-section))
      ;; Now remove anything that is before the appropriate column
      (setq tmp nil)
      (while words 
        (cond
         ((>= (cdar words) column)
          (setq tmp (cons (car words) tmp))))
        (setq words (cdr words)))
      (setq words (reverse tmp))
      (setq result "")
      (while words
        (setq result (concat result (caar words)))
        (cond ((or (string= "\n" (caar words))
                   (string= "\r" (caar words))
                   (string= "\r\n" (caar words)))
               (setq result (concat result " * "))))
        (setq words (cdr words)))
      result
      )
  )


(defun midtech-convert-old-function-header()
    "Convert an old-style midtech function header comment to the 
new doxygen style."
  (interactive)
  (let ((return-descr "") (short-descr "") (long-descr "") 
        (notes "") (params nil) (out-params nil)
        start end section end-section word-start word-end
        param-format
        column found
        tmp)
    ;; First let's find the begining of the comment block
    (end-of-line)
    (setq start (re-search-backward "//[ \t]*\\+"))
    (setq end   (re-search-forward "//[ \t]*-"))
    
    
    
    (goto-char start)
    
      ;; Get the return val description    
      (setq section (re-search-forward "//[ \t]+Return Val[ \t]*:[ \t]*" end))
      (re-search-forward "//[ \t]+Exceptions[ \t]*:[ \t]*" end)
      (previous-line 1)
      (setq return-descr (buffer-substring-no-properties section (line-end-position)))
      
      
      ;; Get the short description
      (goto-char start)
      (setq section (re-search-forward "//[ \t]+Desc[ \t]*:[ \t]*" end))
      (setq short-descr 
            (cond 
             ((re-search-forward "\\." (line-end-position) t)
              (buffer-substring-no-properties section (point)))
             (t (buffer-substring-no-properties section (line-end-position)))))
      
      
      ;; Get the long description
      (goto-char start)
      (setq section (re-search-forward "//[ \t]+Desc[ \t]*:[ \t]*" end))
      (setq column (midtech-current-column))
      (re-search-forward "//[ \t]+Notes[ \t]*:[ \t]*" end)
      (previous-line 1)
      (end-of-line)
      (setq end-section (point))
      (setq long-descr (midtech-comment-words-after-column column 
                                                           section 
                                                           end-section))

      ;; Get the notes section
      (goto-char start)
      (setq section (re-search-forward "//[ \t]+Notes[ \t]*:[ \t]*" end))
      (search-forward "// *************")
      (previous-line 1)
      (setq end-section (line-end-position))
      (setq notes (midtech-comment-words-after-column column 
                                                           section 
                                                           end-section))
    
      (setq params (midtech-find-parameter-description
                       start
                       end
                       "in"
                       nil
                       "//[ \t]+Input[ \t]*:[ \t]*" 
                       "//[ \t]+Output[ \t]*:[ \t]*"))
      (setq params (midtech-find-parameter-description
                       start
                       end
                       "out"
                       params
                       "//[ \t]+Output[ \t]*:[ \t]*"
                       "//[ \t]+Return Val[ \t]*:[ \t]*"))
           
      
      
      ;; Write the new comment
      (goto-char start)
      (insert "/**\n")
      (insert (format " * %s\n" short-descr))
      (insert (format " * \n"))
      (insert (format " * %s\n" long-descr))
      (insert (format " * \n"))
      (when (or
             (> (length notes) 8)
             (and (> (length notes) 4)
                  (not (string-equal (substring notes 0 4) "None"))))
        (insert (format " * @note %s\n" notes))
        (insert (format " * \n")))
      
      
      (when (car params)
        (let (len format-string)
          (setq len (+ 1 (car (sort (mapcar (lambda (expr) (length (car expr)))
                                            params)
                                    '>))))
          
          (setq format-string (format " * @param %%-%ds %%-8s %%s\n" len))
          
          
          (mapcar (lambda (expr) 
                    (insert 
                     (format format-string                   
                             (first expr)
                             (format "(%s)" (third expr))
                             (second expr)
                             )))
                  params)
          (insert (format " * \n"))
          ))
      
      (insert (format " * @return %s \n" return-descr))
      (insert " */\n")
      (setq end (point))
      
      ;; Now convert the "//" comment symbols to just a "*" symbol
      (goto-char start)
      (while (search-forward "//" end t)
        (replace-match " *" nil t))
      
      (goto-char end)
      (previous-line 1)
      (end-of-line)
      (setq start (point))
      (setq end (re-search-forward "//[ \t]*-"))
      (delete-region start end)
    )
)

(defun midtech-current-column()
  "Return column in which the cursor is located."
  (- (point) (line-beginning-position)))

(defun insert-usb-dtype-string(var-name usb-string)
  "Prompt for a name and insert USB string definition before current point"
  (interactive (list
                (read-string (format "Id: "))
                (read-string (format "USB String (be careful with trailing spaces): "))))

  (let* ((start-pos (point)))
    (insert (format " constexpr std::array<uint8_t, %d> %s = {\n" (+ 2 (* 2 (length usb-string))) var-name))
    (insert (format " 2 + %d*2, \n" (length usb-string)))
    (insert " USB_DTYPE_STRING,\n")
    (insert (format "  // '%s'\n" usb-string))

    (insert (mapconcat '(lambda (c) (format "'%c', 0" c)) usb-string ", "))
    
    (insert "\n };\n")
    (insert
     (format "      static_assert(UsbHelpers::validateUsbStringDescriptor(%s), \"validate\");\n" var-name))

    (end-of-line)
    (indent-region start-pos (point) nil)
    )
  )
  
(cond 
 ((string-match "XEmacs" emacs-version)
  (defun line-end-position ()
    (save-excursion
      (end-of-line)
      (point)))
  )
 )
