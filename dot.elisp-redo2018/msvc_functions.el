;;;;
;;;; Copyright 2001, 2002, 2004, 2005, 2009, 2012, 2013, 2018 Craig Spannring
;;;;
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 3. The name of Craig Spannring may not be used to endorse or promote
;;;;    products derived from this software without specific prior
;;;;    written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY CRAIG SPANNRING ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL CRAIG SPANNRING BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.
;;;;


;;;;
;;;; Functions
;;;;   msvc-current-project
;;;;      Display the name of the current project
;;;;
;;;;   msvc-load-project
;;;;      This function will prompt you for the name of a project file
;;;;      It will read in the prject and the prompt you for the name
;;;;      of a configuration (you may hit TAB for a list of choices).
;;;;
;;;;      It can load any of the following project files:
;;;;          .dsp      a MSVC++ workspace file
;;;;          .vcp      a MS eVC workspace file
;;;;          .pbp      a MS WinCE Platform Builder workspace file
;;;;          .depend   a .depend file created by mkdep or gcc -MD
;;;;          .ewp      IAR Embedded Workbench workspace XML files
;;;;      The primary purpose of this function is to retrieve data
;;;;      from the project file to use in the other functions in this
;;;;      package.
;;;;
;;;;   msvc-compile-current-project
;;;;      Compile the current project.
;;;;
;;;;      Note- You must first set the current project using msvc-load-project.
;;;;
;;;;   msvc-find-other-file
;;;;      Find the header or source file corresponding to this file.
;;;;      The current window will switch to the other file.
;;;;
;;;;      If there is no current project file loaded, or if the other
;;;;      file isn't found in the project's file list, then this
;;;;      function will call ff-find-other-file
;;;;
;;;;   msvc-find-file
;;;;      Switch to a buffer that is visiting a file in the current project.
;;;;
;;;;   msvc-doxygen
;;;;      Run doxygen on all the C/C++ source files in the current project
;;;;
;;;;   msvc-create-ebrowse-file
;;;;      Create an ebrowse file on all the C/C++ source files in
;;;;      the current project
;;;;
;;;; $Header: /home/cts/PersonalRepository.tmp/.elisp/msvc_functions.el,v 1.79 2012/02/08 19:37:32 cts Exp $
;;;;


(require 'cl)
(cond
 ((require 'avltree nil t)
  t)
 (t
  (setq load-path (cons '"~/.elisp/elib-1.0" load-path))
  (require 'avltree)))
(require 'xml)
(require 'uvision_project)
(require 'rtags_project)


(defconst msvc-c-source-extensions '("c" "C" "cpp" "CPP")
  "Set of extensions used for C source code files.")
(defconst msvc-c-header-extensions '("h" "H" "hpp" "HPP")
  "Set of extensions used for C header files.")
(defconst msvc-c-template-extensions '("tmpl.h" "TMPL.H" "tmpl.hpp" "TMPL.HPP" "tcc" "TCC")
  "Set of extensions used for C template files.")
(defconst msvc-system-headers '("/usr/include"
                                "/usr/lib"
                                "/usr/lib/include"
                                "/usr/lib/lib"
                                "/usr/local/include"
                                "/usr/local/lib"
                                "/usr/local/embedded-arm-cross")
  "Set of directories that contain 'system' header files.")
(defconst msvc-ebrowse-exe "ebrowse"
  "Name of the ebrowse executable.  If ebrowse is not in your PATH then you must give the full pathname to the executable")

(defconst msvc-gtags-exe "gtags"
  "Name of the gtags executable.  If gtags is not in your PATH then you must give the full pathname to the executable")

(defcustom xcodebuild "xcodebuild"
  "Name of the xcode build program.  If this isn't in your path then
you must give the full pathname to the executable.")

(defcustom msvc-uvbuild-exe 
  (cond 
   ((file-exists-p "c:\\BuildTools\\build_lpc_project.py")
    "python c:\\BuildTools\\build_lpc_project.py")
   ((file-exists-p "c:\\LPCFamily\\Build\\build_project.py")
    "python c:\\LPCFamily\\Build\\build_project.py")
   (t
    "python build_project.py"))
  "Name of the Keil uvision command line build program.")

(defconst msvc-iarbuild-exe
  (cond
   ((file-exists-p "c:\\IARSystems\\EmbeddedWorkbench4.0\\common\\bin\\iarbuild.exe")
    "c:\\IARSystems\\EmbeddedWorkbench4.0\\common\\bin\\iarbuild.exe")
   (t "iarbuild.exe"))
  "Name of the IAR build program.  If the IAR compiler is not in your PATH
then you must give the full pathname to the executable")

(defconst msvc-msdev-exe "msdev.exe"
  "Name of the Visual C++ IDE program.  If the IDE is not in your PATH
then you must give the full pathname to the executable")

(defconst msvc-cepb-exe "C:\\PROGRA~1\\WI2EF7~1\\4.20\\cepb\\bin\\cepb.com"
  "Name of the Microsoft Platform Builder IDE program.  If the IDE is not in your PATH
then you must give the full pathname to the executable")

(defconst msvc-evc4-exe
; "c:\\progra~1\\mic604~1\\common\\evc\\bin\\evc.exe"
"c:\\PROGRA~1\\MICROS~1.0\\Common\\EVC\\Bin\\evc.exe"

  "Name of eMbedded Visual C++ 4.0 IDE program.  If the IDE is not in your PATH
then you must give the full pathname to the executable")
(defconst msvc-evc3-exe
 "c:\\progra~1\\micros~4\\common\\evc\\bin\\evc.exe"

  "Name of eMbedded Visual 3.0 C++ IDE program.  If the IDE is not in your PATH
then you must give the full pathname to the executable")

(defconst msvc-evc-default-ceconfig "MAXALLTT"
  "Name of the Windows CE configuration to work with.  Used
only with eMbedded Visual C++ projects")

(defvar msvc-doxygen-exe
  (if (string= system-type "windows-nt") "doxygen.exe" "doxygen")
  "Name of the doxygen executable")

(defvar msvc-dot-path
  ""
  "Path the dot executable.  This can be left empty if dot is in the path")

(defvar msvc-cccc-exe
  (if (string= system-type "windows-nt") "cccc.exe" "cccc")
  "Name of the cccc executable")

(defconst msvc-evc-configurations-executable-mappings
  (list (list "MAXALLTT"    msvc-evc3-exe)
        (list "STANDARDSDK" msvc-evc4-exe)
        (list "YATWCE_2"    msvc-evc4-exe)
        (list "MIDTECH7760" msvc-evc4-exe)
        (list "JettCE"      msvc-evc4-exe))
  "List of each supported eVC configuration and the path to the evc.exe
program that should be used to conpile that configuration")

; Set msvc-doxygen-template constant
(require 'msvc-doxygen-template)


(defun msvc-convert-unix-file-name-to-windows (path-name)
  "Convert from a unix-style pathname to windows style pathname 

   Currently this just replaces all forward slashes with back slashes
   "
  (let* ((components (remove-if (lambda (x) (= 0 (length x)))
                                (split-string path-name "/")))
         (new-path (mapconcat 'identity components "\\")))
    new-path))


(defvar msvc-read-file-name-with-extension-list nil)

(defvar msvc-current-compilation-system nil
  "Compilation system that we are using.  Should be either 'gmake, 'rtags-ide, 'iar, 'uvision, 'msvc, or 'other")

(defvar msvc-project-directory nil
  "Directory where the project file lives.

  Note- Some systems (e.g. xcode) the 'project' file is
        actually a directory.  In this case the project
        directory is the directory containing the
        project directory.  (e.g. For project
             /home/cts/Proj/myprog.xcodeproj/
        msvc-project-directory will be the string '/home/cts/Proj'
")

(defvar msvc-project-file-tree nil
  "AVL tree that allows you to find the corresponding header file for
a given source file and viceversa.

The items in the data are of the form (basename source header template))
")

(defvar msvc-project-file-list nil
  "List of all files in the project")

(defvar msvc-evc-ceconfig nil)
(defvar msvc-current-ide nil)


(defun _msvc-system-headerp (fname)
  "Is the file in one of the system header directories?

FNAME - Fully qualified path of a file. "
  (let* ((result (reduce (lambda (a b) (or a b))
                         (mapcar (lambda (x)
                                    (let* ((tmp (string-prefix-p x fname)))
                                      ;; (message "%s,%s -> %s" fname x tmp)
                                      tmp))
                                 msvc-system-headers))))
    ;; (message "_msvc-system-headerp %s -> %s" fname result)
    result))



;;;;
;;;; file lookup dictionary functions
;;;;

(defun _msvc-filelookup-new ()
  "Create a new (aka empty) file lookup AVL tree"
   (setq msvc-project-file-tree (avltree-create (lambda (x1 x2)
                                                  (string-lessp
                                                   (downcase (car x1))
                                                   (downcase (car x2)))))))


(defun _msvc-filelookup-filetuples-list ()
  "return a partially flattened list of files

The returned list is of the form ((list of files with common basename1) ...).

Example-
   ((foo.cpp foo.h) (nil bar.h) (glorp.cpp glorp.hpp glorp.tmpl.h))
"
  (mapcar (lambda (x) (rest x)) ; (list (cadr x) (cddr x)))
                      (avltree-flatten msvc-project-file-tree)))


(defun _msvc-filelookup-allfiles ()
  "return a list of all files in the dictionary

   (\"file1\" \"file2\" ...)
"
   (let ((result nil))
     (defun _msvc-fl-af-help (x)
       (cond ((null x)    nil)
             (t
              (when (car x) (setq result (cons (car x) result)))
              (_msvc-fl-af-help (cdr x)))))
     (mapc '_msvc-fl-af-help (_msvc-filelookup-filetuples-list))
     result))


(defun _msvc-filelookup-files-alist ()
  "Create a alist that maps base filenames to fully qualified file names

The result is of the form ((\"Foo.h\" . \"/fullpath/Foo.h\") ...)
"
  (mapcar (lambda (fname)
            (let* ((basename (file-name-nondirectory fname)))
              ;;(message "processing %s" fname)
              ;;(message "  result %s" (cons basename fname))
              (cons basename fname)))
          (_msvc-filelookup-allfiles)))


(defun _msvc-filelookup-add-to-tree (fullname)
  "Add a file to msvc-project-file-tree"
  ;; (message "%s (_msvc-filelookup-add-to-tree %s)" (float-time) fullname)
  (let* ((start-time  (float-time))
         (key         (_msvc-file-name-key fullname))
         (extension   (_msvc_file-name-extension fullname))
         (is-source   (member extension msvc-c-source-extensions))
         (is-header   (member extension msvc-c-header-extensions))
         (is-template (member extension msvc-c-template-extensions))
         (item      (list key
                          (cond (is-source    fullname)
                                (t            (_msvc-filelookup-source key)))
                          (cond (is-header    fullname)
                                (t            (_msvc-filelookup-header key)))
                          (cond (is-template  fullname)
                                (t            (_msvc-filelookup-template key))))))
    ;; (message "%s Loading: %s" (float-time) (file-name-nondirectory fullname))
    (message "Loading: %s"  (file-name-nondirectory fullname))
    (avltree-enter msvc-project-file-tree item)))


(defun _msvc-filelookup-template (fname)
  "Given a filename, return the corresponding header file name.

FNAME can be either the fully qualified name in the project or a
key in the msvc-project-file-tree dictionary.

The result will be a fully qualified pathname (string) of the header file
"

  (let* ((item  (_msvc-filelookup-get-item (_msvc-file-name-key fname))))
    (cond ((and item (third item))  (third item))
          (t                        nil))))


(defun _msvc-filelookup-header (fname)
  "Given a filename, return the corresponding header file name.

FNAME can be either the fully qualified name in the project or a
key in the msvc-project-file-tree dictionary.

The result will be a fully qualified pathname (string) of the header file
"

  (let* ((item  (_msvc-filelookup-get-item (_msvc-file-name-key fname))))
    (cond ((and item (second item))  (second item))
          (t                        nil))))


(defun _msvc-filelookup-source (fname)
  "Given a filename, return the corresponding source file name.

FNAME can be either the fully qualified name in the project or a
key in the msvc-project-file-tree dictionary.

The result will be a fully qualified pathname (string) of the header file
"

  (let* ((key   (_msvc-file-name-key fname))
         (item  (_msvc-filelookup-get-item key)))
    (cond ((and item (first item)) (first item))
          (t                        nil))))


(defun _msvc-filelookup-get-item (key)
  "Lookup key in the avltree.
Return a list with the matching filename, nil if key is not in tree. "

  (rest (avltree-member msvc-project-file-tree (list key nil nil nil))))



;;;;
;;;; end of file lookup dictionary functions
;;;;


(defun _msvc-file-name-key (fname)
  "Return a string suitable for the key in the msvc-project-file-tree AVL tree"
  (let* ((clean-name   (file-name-sans-versions (file-name-nondirectory fname)))
         (extension    (_msvc_file-name-extension clean-name t))
         (base-length  (- (length clean-name) (length extension))))
    (downcase (substring clean-name 0 base-length))))


(defun _msvc-ends-with (str suffix)
  (cond ((>= (length str) (length suffix))
         (string= suffix (substring str (- (length str) (length suffix)))))))


(defun _msvc_file-name-extension (filename &optional period)
  "Return FILENAME's final \"extension\".
The extension, in a file name, is the part that follows the last `.'.
Return nil for extensionless file names such as `foo'.
Return the empty string for file names such as `foo.'.

If PERIOD is non-nil, then the returned value includes the period
that delimits the extension, and if FILENAME has no extension,
the value is \"\".
"
  (let*  ((result               nil))
    ;; the mapc call is just going search through all of the
    ;; extensions in mscv-c-template-extension.  If it finds
    ;; one that matches the extension on filename it will setq result
    ;; to the extension.
    ;;
    ;; Note- we have to check for C++ template files first because
    ;; they are, at least at TeeJet, of the form .tmpl.h.  If we
    ;; searched for .h before .tmpl.h we'd get just the .h portion.
    ;; What we want is the .tmpl.h
    (mapc (lambda (ext)
            (when (_msvc-ends-with filename ext) (setq result ext)))
          msvc-c-template-extensions)

    ;; If none of the template extensions matched then search for the
    ;; normal filename extensions.
    (when (not result)
      (setq result (save-match-data
                     (let ((file    (file-name-sans-versions (file-name-nondirectory
                                                              filename))))
                       (if (string-match "\\.[^.]*\\'" file)
                           (substring file (+ (match-beginning 0) 1)))))))
    (cond ((and result period) (concat "." result))
          (t                   result))))


(defun msvc-current-project ()
  "Display the current project at the bottom on the screen"
  (interactive)
  (cond
   ((boundp 'msvc-current-project)
    (message "%s" msvc-current-project))
   (t
    (message "No Project"))))


(defun msvc-complete-all-file-names (simple-name directory predicate)
  ;;
  ;; Get a list of possible matches and remove any elements that
  ;; aren't either directories or projects
  ;;
  (remove nil
          (mapcar (lambda (x)
                    ;; The 'and' function returns 'x' if the 'or' part is true
                    (and (or
                          (file-directory-p (concat directory x))
                          (funcall predicate (concat directory x)))
                         x))
                  (file-name-all-completions simple-name
                                             (if directory directory "")))))


(defun msvc-complete-file-name (name predicate how)
  ;; First get rid of anything in front of the last double slash
  (while (string-match "//" name)
    (setq name (substring name (+ 2 (string-match "//" name)))))
  ;; When reading the following code, remember that backslash (\) is
  ;; special both in strings and in regexps
  (while (string-match "\\\\\\\\" name)
    (setq name (substring name (+ 2 (string-match "\\\\\\\\" name)))))


  (setq name (file-truename name))

  (let (extension-okay
        (result       (file-truename name))
        (directory    (file-name-directory (file-truename name)))
        (simple-name  (file-name-nondirectory (file-truename name))))

    (setq directory (if directory directory ""))


    (cond
     ((equal how t)
      (msvc-complete-all-file-names simple-name directory predicate))
     ((eval (list predicate result))
      ;; Great, we've found it
      t)
     ((file-name-all-completions simple-name
                                 (if directory directory ""))
      (let ((possibles (msvc-complete-all-file-names simple-name directory predicate)))
        (cond
         ((not possibles)
          ;; If nothing's left then fail.
          nil)
         ((= (length possibles) 1)
          ;;
          ;; If we have only one element left return it as the result
          ;;
          (concat (if directory directory "") (car possibles)))
         (t
          ;;
          ;; otherwise find the longest common substring
          ;;
          (setq result
                (concat (if directory directory "")
                        (try-completion simple-name
                                        (mapcar (lambda (y) (cons y 1))
                                                possibles))))
          result))))
     (t
      nil))))


(defun msvc-read-file-name-with-extension-p (name)
  ;; (message "looking at %s" name)
  (let* ((result
          (cond
           ((not (_msvc_file-name-extension name))
            nil)
           ((_msvc_file-name-extension name)
            (and
             (member t (mapcar (lambda (x)
                                 (equal (downcase x)
                                        (downcase (_msvc_file-name-extension name))))
                               msvc-read-file-name-with-extension-list))
             (file-exists-p name)
             (file-regular-p name)))
           (t
            ;; (message "hit the defualt case")
            nil))))
    ;; (message "looking at %s -> %s" name result)
    result))



(defun msvc-read-file-name-with-extension (prompt dir extensions initial)
  (setq  msvc-read-file-name-with-extension-list extensions)
  (completing-read
   prompt                                ; prompt
   'msvc-complete-file-name              ; table
   'msvc-read-file-name-with-extension-p ; predicate
   t                                     ; require-match
   nil                                   ; init
   nil                                   ; hist
   nil)                                  ; def
  )

(defun msvc-read-dep-file-name-p (name)
  "Predicate that returns true if NAME is a gnumake dependecy file (i.e. .depend)"

  (let (result)
    (setq result
          (cond
           ((and
             (string= (file-name-nondirectory name) ".depend")
             (file-exists-p name)
             (file-regular-p name))
            t)
           ((not (_msvc_file-name-extension name))
            nil)
           ((_msvc_file-name-extension name)
            (and
             (string= (file-name-nondirectory name) ".depend")
             (file-exists-p name)
             (file-regular-p name)))
           (t nil)))
    result))


(defun msvc-read-dep-file-name (prompt dir extensions initial)
  (completing-read
   prompt                               ; prompt
   'msvc-complete-file-name             ; table
   'msvc-read-dep-file-name-p           ; predicate
   t                                    ; require-match
   nil                                  ; init
   nil                                  ; hist
   nil))                                ; def

(defun set-compile-diretory (dir)
  "Set the compile directory.

   This command is useful if you want to use the
   msvc-compile-current-project command for projects based on
   build systems not supported by this module.
   "

  (interactive "D")
  (message "Compile directory is %s" dir)
  (setq msvc-current-project nil)
  (setq msvc-project-directory dir)
  (setq msvc-current-compilation-system 'other)
  (message "Default compile directory %s" dir)
)

(defun _msvc-setup-project (proj-file
                            build-configuration
                            compilation-system
                            source-files)
  (let* ((proj-dir (file-name-directory proj-file)))
    ;; (setq msvc-project-directory          proj-dir)
    (_msvc-filelookup-new)
    (mapc (lambda (x) (msvc-add-file-to-project-data x proj-dir))
          source-files)

    (setq msvc-project-directory          proj-dir)
    (setq msvc-current-project            proj-file)
    (setq msvc-current-config             build-configuration)
    (setq msvc-current-compilation-system compilation-system)
    (setq msvc-project-file-list          (_msvc-filelookup-allfiles))))


(defun msvc_iar-expand-filename (name proj-dir)
  "Replace the $PROJ_DIR$ prefix in a filename

   NAME and PROJ-DIR should each be a string
   "
  (assert (eq (type-of name) 'string))
  (assert (eq (type-of proj-dir) 'string))

  (dolist (prefix '("$PROJ_DIR$\\" "$PROJ_DIR$/") name)
    (when (and (> (length name)
                  (length prefix))
               (string= prefix (substring name 0 (length prefix))))
      (setq name (substring name (length prefix)))
      (setq name (concat proj-dir name))))
  (setq name (expand-file-name name))
  name)


(defun msvc-clear-text-props (x)
  (set-text-properties 0 (length x) nil x)
  x)

(defun msvc_iar-filename (file-node)
  "Retrieve the file name from the xml node"
  (let ((name-node (xml-get-children file-node 'name)))
    (let ((result (caddar name-node)))
      (msvc-clear-text-props result)
      result)))


(defun msvc_iar-get-dep-configuration-names (proj-node)
  "Return a list of the configurations found in the IAR EW project"

  (assert (not (consp (car proj-node))))    ; It should be just a single xml node
  (assert (equal (car proj-node) 'project)) ; It should be a project node
  (mapcar (lambda (x)  (caddar (xml-get-children x 'name)))
            (xml-get-children proj-node 'configuration)))



(defun msvc_iar-get-file-list-from-proj-node (parent-node)
  "Return a list of file found in either an IAR project xml node or a group node"

  (assert (not (consp (car parent-node))))    ; It should be just a single xml node
  (assert (or
           (equal (car parent-node) 'project) ; It should be a project node
           (equal (car parent-node) 'group)))  ; or maybe a group node

  (let ((result nil))

    ;;
    ;; First we'll just get the files listed in the topmost level
    ;;
    (let (file-node
          (file-nodes (xml-get-children parent-node 'file)))
      (while (setq file-node (pop file-nodes))
        (push (msvc_iar-filename file-node) result)))

    ;;
    ;; Now get the files in the group nodes.  Note- we could have groups nested
    ;; inside groups.
    ;;
    (let (tmp
          group-node
          (group-nodes (xml-get-children parent-node 'group)))
      (while (setq group-node (pop group-nodes))
        (setq tmp (msvc_iar-get-file-list-from-proj-node group-node))
        (when tmp (setq result (append result tmp)))))
    result
  )
)

(defun msvc-iar-get-dep-files (dep-file cfg)
  "Return a list of dependency files from a IAR Embedded Workbench .dep file"

  (let ((proj-node (car (xml-parse-file dep-file)))
        cfg-node       ; node containing the desired configuration
        file-list
        file-version
        foo)
    (assert (type-of cfg) 'string)
    (assert (not (consp (car proj-node))))    ; It should be just a single xml node
    (assert (equal (car proj-node) 'project)) ; It should be a project node

    ;;
    ;; Find the file version specified by cfg
    ;;
    (setq file-version (caddar (xml-get-children proj-node 'fileVersion)))
    (cond ((string= file-version "1") (msvc-iar-get-dep-files-version-1 dep-file cfg))
          ((string= file-version "2") (msvc-iar-get-dep-files-version-2 dep-file cfg))
          (t                          (msvc-iar-get-dep-files-version-2 dep-file cfg)))))


(defun msvc-iar-get-dep-files-version-1 (dep-file cfg)
  "Return a list of dependency files from a version 1 IAR Embedded Workbench .dep file"

  (let ((proj-node (car (xml-parse-file dep-file)))
        cfg-node       ; node containing the desired configuration
        file-list
        foo)
    (assert (type-of cfg) 'string)
    (assert (not (consp (car proj-node))))    ; It should be just a single xml node
    (assert (equal (car proj-node) 'project)) ; It should be a project node

    ;;
    ;; Find the configuration node specified by cfg
    ;;
    (dolist (tmp-node (xml-get-children proj-node 'configuration))
      (when (string= cfg (caddar (xml-get-children tmp-node 'name)))
        (setq cfg-node tmp-node)))

    ;;
    ;; Go through each file looking for file with the tool "ICC*" in the
    ;; outputs node.
    ;;
    (dolist (file-node (xml-get-children cfg-node 'file))
      (dolist (inputs-node (xml-get-children file-node 'inputs))
        (dolist (tool-node (xml-get-children inputs-node 'tool))
          (let ((tool-name (caddar (xml-get-children tool-node 'name))))
            (when (string-match "^ICC" tool-name)
              (dolist (file-node (xml-get-children tool-node 'file))
                (push (msvc-clear-text-props (caddr file-node)) file-list)
))))))
    file-list))


(defun msvc-iar-get-dep-files-version-2 (dep-file cfg)
  "Return a list of dependency files from a version 2 IAR Embedded Workbench .dep file"

  (let ((proj-node (car (xml-parse-file dep-file)))
        cfg-node       ; node containing the desired configuration
        file-list
        file-nodes
        tmp)

    (assert (type-of cfg) 'string)
    (assert (not (consp (car proj-node))))    ; It should be just a single xml node
    (assert (equal (car proj-node) 'project)) ; It should be a project node

    ;;
    ;; Find the configuration node specified by cfg
    ;;
    (dolist (tmp-node (xml-get-children proj-node 'configuration))
      (when (string= cfg (caddar (xml-get-children tmp-node 'name)))
        (setq cfg-node tmp-node)))

    ;;
    ;; I'm unsure about what's in version 2 of XML file.  It really doesn't
    ;; sound right, but for now we'll grab all of the files listed in the
    ;; outputs section and add whatever .h files we see
    ;;
    (setq file-nodes (xml-get-children (car (xml-get-children cfg-node 'outputs)) 'file))
    (dolist (f (xml-get-children file-nodes 'file))
      (setq tmp (msvc-clear-text-props (caddr f)))
      (cond ((string-match ".*\\.[hH]\\>" tmp)
             (push tmp file-list))))

    file-list))


(defun msvc-iar-load-project  (proj-file)
  "Load in a IAR Embedded Workbench project file"

  (interactive (list
                (expand-file-name
                 (msvc-read-dep-file-name
                  "Project File: "
                  nil
                  '("ewp")
                  nil))))
  (let ((proj-node (car (xml-parse-file proj-file))) ; xml node containing IAR project
        (proj-name (file-name-sans-extension (file-name-nondirectory proj-file)))
        (proj-dir  (file-name-directory proj-file))
        dep-file             ; IAR dependendencies file (The .dep file)
        cfg                  ; Name of configuration the user wants to use      (list of string)
        config-names         ; List of the configurations found in project file (list of strings)
        dep-files-list       ; List of header files (i.e. dependencies)
        file-list)           ; List of file names (list of string)

    (assert (not (consp (car proj-node))))    ; It should be just a single xml node
    (assert (equal (car proj-node) 'project)) ; It should be a project node


    ;;
    ;; figure out what configurations are in this project
    ;; and set the defualt configuration to the first one
    ;; contained in the project file
    ;;
    (setq config-names (msvc_iar-get-dep-configuration-names proj-node))
    (setq cfg (car config-names))
    (setq cfg (completing-read
               (format "Configuration (%s): " cfg)           ; prompt
               (mapcar (lambda (x) (cons x x)) config-names) ; table
               nil                                           ; predicate
               t                                             ; require match
               ))
    (when (or (not cfg) (= (length cfg) 0)) (setq cfg (car config-names)))
    (setq msvc-current-config cfg)

    ;;
    ;; Get the list of soruce file names from the project file
    ;;
    (setq file-list (msvc_iar-get-file-list-from-proj-node proj-node))

    ;;
    ;; If the .dep file exists we should get a list of source and header files
    ;; contained in that file.  Append those to the file-list.
    ;;
    (setq dep-file (expand-file-name (concat proj-name ".dep") proj-dir))
    (when (not (file-readable-p dep-file)) (setq dep-file nil))
    (when dep-file (setq dep-files-list (msvc-iar-get-dep-files dep-file cfg)))
    (when dep-files-list (setq file-list (append file-list dep-files-list)))


    ;;
    ;; file-list contains the source and header files in the project.
    ;; We need to clean up the file names, filter out the ones we aren't
    ;; interested in and store those file names in msvc-project-file-list
    ;; and msvc-project-file-tree.
    ;;
    (setq msvc-project-directory proj-dir)
    (setq msvc-project-file-list nil)

    (dolist (file-name file-list)
      (let ((ext (_msvc_file-name-extension file-name)))
        ;; Ignore system include files.
        (when (not (string-match "^\\$TOOLKIT_DIR\\$" file-name))
          (setq file-name (msvc_iar-expand-filename file-name proj-dir))
          (cond
           ((< (length file-name) 1)     nil)            ; Not interested in empty strings.  skip
           ((string= (substring file-name 0 1) "$") nil) ; Not interested in strings that start with $.  skip
           ((and ext (member (downcase ext) '("cpp" "c" "h" "hpp" "cxx" "hxx")))
            (when (not (member file-name msvc-project-file-list))
              (push file-name msvc-project-file-list)))))))


    (_msvc-filelookup-new)

    (mapc
     (lambda (x) (msvc-add-file-to-project-data x msvc-project-directory))
     msvc-project-file-list)

    (setq msvc-current-project proj-file)
    (setq msvc-project-directory proj-dir)
    (setq msvc-current-compilation-system 'iar)
    (_msvc-possibly-setup-midtech-header-defaults "rowbot")
    (message "Loaded %s" proj-name)))


(defun msvc-rtags-config-load-project (proj-file)
  "Load rtags project file for an IDE in Emacs"

  (interactive (list
                (expand-file-name
                 (msvc-read-dep-file-name
                  "Project File: "
                  nil
                  '("rtag-config")
                  nil))))
  (let* ((proj-path   (file-name-directory (expand-file-name proj-file)))
	 (commands-db (concat proj-path "compile_commands.json")))
    (when (or
	   nil
	   (not (file-exists-p commands-db)))
      (error "Missing compile_commands.json.  Perhaps run 'bear make ...'"))
    (cts-rtp-start-rdmserver-unless-running proj-path)
    (dotimes (i 5)
      (let ((running (cts-rtp--is-server-running proj-path))
            (responsive (cts-rtp--is-server-responsive proj-path)))
        (when (not (and running responsive))
          (message "Waiting for RDM server to start")
          (sleep-for 1))))
    (unless (cts-rtp--is-server-responsive proj-path)
      (error "Could not start RDM server"))
    
    (setq msvc-current-project proj-file)
    (setq msvc-project-directory (file-name-directory proj-file))

    (cts-rtp--load-compile-commands proj-path)
    (cts-rtp-switch-project proj-path)
    (setq msvc-current-compilation-system 'rtags-ide)))


(defun msvc-uvproj-load-project (proj-file)
  "Load a Kiel uvision project file"

  (interactive (list
                (expand-file-name
                 (msvc-read-dep-file-name
                  "Project File: "
                  nil
                  '(".uvproj")
                  nil))))
  (let* ((proj-dir       (file-name-directory proj-file))
         (proj-name      (file-name-sans-extension (file-name-nondirectory proj-file)))
         (config-names   (uv-get-build-targets-from-file proj-file)) ; list of build configurations (strings)
         (cfg            (completing-read                             ; Name of the choosen build configuration
                              (format "Configuration (%s): " (car config-names))
                              (mapcar (lambda (x) (cons x x)) config-names)
                              nil
                              t
                              nil
                              nil
                              (car config-names)))
         (dep-file       (expand-file-name (format "%s_%s.dep" proj-name cfg) proj-dir))
         (source-files   (uv-get-source-files proj-file dep-file)))

    (_msvc-setup-project proj-file cfg 'uvision source-files)))


(defun msvc-vs2005-load-project (proj-file)
  "Load in a Visual Studio 2005 project file"

  (interactive(list
                (expand-file-name
                 (msvc-read-dep-file-name
                  "Project File: "
                  nil
                  '("vcproj")
                  nil))))

  (let ((proj-node (car (xml-parse-file proj-file))) ; xml node containing project
        (proj-name (file-name-sans-extension (file-name-nondirectory proj-file)))
        (proj-dir  (file-name-directory proj-file))
        cfg                  ; Name of configuration the user wants to use      (list of string)
        config-names         ; List of the configurations found in project file (list of strings)
        dep-files-list       ; List of header files (i.e. dependencies)
        file-list)           ; List of file names (list of string)

    (assert (not (consp (car proj-node))))    ; It should be just a single xml node
    (assert (equal (car proj-node) 'VisualStudioProject)) ; It should be a project node

    (message "about to call msvc_vs2005-get-configuration-names")
    (setq config-names (msvc_vs2005-get-configuration-names proj-node))

    (message "configuration names are %s" config-names)
))


(defun msvc_vs2005-get-configuration-names (proj-node)
  "Return a list of the configurations found in the IAR EW project"

  (assert (not (consp (car proj-node))))    ; It should be just a single xml node
  (assert (equal (car proj-node) 'VisualStudioProject)) ; It should be a project node
  (print "inside msvc_vs2005-get-configuration-names")

;(message "here")
;(xml-get-children proj-node proj-node)
;(message "back")

  (let ((configurations-node (xml-get-children proj-node 'Configurations)))
    (message "configurations-node is %s" (car configurations-node))
    (print (format "configurations-node has %d children\n" (length (xml-node-children (car configurations-node)))))
    (mapcar (lambda (x) (print (format "x is %s\n" x)) x)
            (xml-node-children (car configurations-node)))

    ;;         (xml-get-children proj-node 'configuration))
    )
)



(defun msvc-gmake-load-depend-file (dep-file)
  "Load in a .depend file created by mkdep"

  (message "entering msvc-gmake-load-depend-file with %s" dep-file)
  (interactive(list
                (expand-file-name
                 (msvc-read-dep-file-name
                  "Project File: "
                  nil
                  '("dsp" "vcp")
                  nil))))
  (message "dep-file after calling nteractive is %s" dep-file)
  

  (save-excursion
    (let (history item basename fullname extension
          (proj-buffer (format "*GMAKE-DEP-%s*" (file-name-directory dep-file))))

      (get-buffer-create proj-buffer)
      (set-buffer proj-buffer)

      (insert-file-contents dep-file nil nil nil t)

      (setq msvc-current-project dep-file)
      (setq msvc-project-directory (file-name-directory dep-file))

      (setq msvc-current-compilation-system 'gmake)

      ;; Translate from cygwin paths to DOS paths
      (goto-char (point-min))
      (while (search-forward "/cygdrive/z/" nil t)
        (replace-match "z:/" nil t))
      (goto-char (point-min))
      (while (search-forward "/cygdrive/e/" nil t)
        (replace-match "e:/" nil t))
      (goto-char (point-min))
      (while (search-forward "/cygdrive/d/" nil t)
        (replace-match "d:/" nil t))


      (message "Reading file list...")
      (setq msvc-project-file-list nil)
      (_msvc-filelookup-new)
      (shell-command-on-region (point-min) (point-max) "~/.elisp-redo2018/clean_dot_depend_file.py" nil t)

      (let ((mytime nil)
            (all-files (remove-if (lambda (x) (string= "" x))
                                  (split-string (buffer-substring
                                                 (point-min)
                                                 (point-max))
                                                "[ \t\n\r\v\f\\]+"))))
        (mapc
         (lambda (x)
           ;; Add files that aren't in one of the system header files.
           (setq mytime (float-time))
           (when (not (_msvc-system-headerp x))
             (msvc-add-file-to-project-data x msvc-project-directory))
           )
         all-files))

      (setq msvc-project-file-list (_msvc-filelookup-allfiles))

      (_msvc-possibly-setup-midtech-header-defaults "generic")

      (message "Loaded."))))


(defun msvc-starts-with (s prefix)
  "Does the string S start with the string PREFIX?"
  (string= prefix (substring s 0 (min (length s) (length prefix)))))


(defun msvc-setup-evc-configurations ()
  (let ((sdk (completing-read
              "Configuration: "                                ; prompt
              (mapcar
               (lambda (x) (cons (car x) (car x)))
               msvc-evc-configurations-executable-mappings)))) ; table
    (setq msvc-evc-ceconfig sdk)
    (setq msvc-current-ide
          (cadr (assoc sdk msvc-evc-configurations-executable-mappings)))))

(defun msvc-load-project (proj-file)
  "Load in a mkdep .depend file, a IAR EW .ewp project file, a
   Microsoft Visual C++ project, Platform Builder project
   (.PBP), or eMbedded Visual C project (.VCP) file and use the
   information to determine what source files are in the project and the
   location of the browse information file."

  (interactive (list
                (expand-file-name
                 (msvc-read-file-name-with-extension
                  "Project File: "
                  nil
                  '("vcproj" "dsp" "vcp" "pbp" "depend" "ewp" "xcodeproj" "uvproj" "rtags-config")
                  nil))))
  (message "proj-file is %s" proj-file)
  (cond
   ((string= (_msvc_file-name-extension proj-file) "depend")
    (msvc-gmake-load-depend-file proj-file))
   ((string= (_msvc_file-name-extension proj-file) "ewp")
    (msvc-iar-load-project proj-file))
   ((string= (_msvc_file-name-extension proj-file) "vcproj")
    (msvc-vs2005-load-project proj-file))
   ((string= (_msvc_file-name-extension proj-file) "xcodeproj")
    (message "sorry, not implemented yet"))
   ((string= (_msvc_file-name-extension proj-file) "uvproj")
    (msvc-uvproj-load-project proj-file))
   ((string= (_msvc_file-name-extension proj-file) "rtags-config")
    (msvc-rtags-config-load-project proj-file))
   (t
    (_msvc-set-project proj-file)))
  (global-set-key [f9 ?f] 'rtags-find-file))


(defun _msvc-set-project (proj-file)
  "Load in a Microsoft Visual C++ project (.DSP), Platform Builder
project (.PBP), or eMbedded Visual C project (.VCP) file and use the
information to determine what source files are in the project and the
location of the browse information file."

  (interactive (list
                (expand-file-name
                 (msvc-read-file-name-with-extension
                  "Project File: "
                  nil
                  '("dsp" "vcp" "pbp")
                  nil))))

  (save-excursion
    (let (configurations
          history item basename fullname extension
          (proj-buffer (format "*MSVC-DSP-%s*" (file-name-nondirectory proj-file))))

      (get-buffer-create proj-buffer)
      (set-buffer proj-buffer)

      (insert-file-contents proj-file nil nil nil t)

      ;;
      ;; First make sure that we have something that looks like
      ;; a project file.
      ;;
      (goto-char (point-min))
      (cond
       ((and (goto-char (point-min))
             (search-forward
              "# Microsoft Developer Studio Generated Build File, Format Version 6.00"
              nil t))
        (setq msvc-evc-ceconfig nil)
        (setq msvc-current-ide msvc-msdev-exe)
        (message "Have a MSVC++ project"))
       ((and (goto-char (point-min))
             (search-forward
              "# Microsoft eMbedded Visual Tools Generated Build File, Format Version 6.02"
              nil t))
        (setq msvc-current-ide msvc-evc3-exe)
        (setq msvc-evc-ceconfig msvc-evc-default-ceconfig)
        (message "Have an EVC project"))
       ((and (goto-char (point-min))
             (search-forward
              "# Microsoft Developer Studio Project File - Name=" nil t))
        (setq msvc-current-ide msvc-cepb-exe)
        (setq msvc-evc-ceconfig msvc-evc-default-ceconfig)
        (message "Have an Platform project"))
       (t
        (error "%s is not a MSVC project file." proj-file)))

      (setq msvc-current-project proj-file)
      (setq msvc-project-directory (file-name-directory proj-file))

      ;;
      ;; Ask the user for a configuration
      ;;
      (goto-char (point-min))
      (setq configurations nil)
      (setq history nil)
      (while (search-forward-regexp "^!\\(ELSE\\|\\)IF  \"\\$(CFG)\" == \"" nil t)
        (let (cfg (tmp-point (point)))
          (search-forward "\"")
          (setq cfg (buffer-substring tmp-point (- (point) 1)))
          (when (not (member (cons cfg 1) configurations))
            (setq history (cons cfg history))
            (setq configurations (cons (cons cfg 1) configurations)))))
      ;(setq msvc-current-config (completing-read
      ;                           (format "-Configuration (%s): " (car history)) ; prompt
      ;                           configurations    ; table
      ;                           nil               ; predicate
      ;                           t                 ; require-match
      ;                           nil               ; init
      ;                            (cons 'history 0) ; hist
      ;                             (car history)))     ; def
      (setq msvc-current-config (completing-read
                                 (format "Configuration (%s): " (car history))  ; prompt
                                 configurations                                 ; table
                                 nil                                            ; predicate
                                 t                                              ; require match
                                 nil                                            ; initial contents
                                 nil                                            ; history list
                                 (car history)                                  ; default
                                 )
            )

      ;;
      ;; If this is an eVC++ project then find out which SDK the user wants
      ;;
      (when (and (goto-char (point-min))
                 (search-forward
                  "# Microsoft eMbedded Visual Tools Generated Build File, Format Version 6.02"
                  nil t))
        (msvc-setup-evc-configurations))


      ;;
      ;; We use an AVL tree to store the locations of the header and
      ;; source files. Each node of the tree is of the form
      ;;
      ;;     (basename . (full-path-to-source full-path-to-header)
      ;;
      ;; where basename is the name without the directory path
      ;; or file extension.
      ;;
      (message "Reading file list...")
      (setq msvc-project-file-list nil)
      (_msvc-filelookup-new)

      (goto-char (point-min))
      (while (search-forward-regexp "^SOURCE=" nil t)
        (let* ((fname (buffer-substring (point) (line-end-position))))
          (message "looking at adding %s" fname)
          (msvc-add-file-to-project-data fname msvc-project-directory)))
      (setq msvc-project-file-list (_msvc-filelookup-allfiles))))
  (setq msvc-current-compilation-system 'msvc)
  (cond
   ((string= (file-name-extension proj-file) "vcp") (_msvc-possibly-setup-midtech-header-defaults "evc4"))
   ((string= (file-name-extension proj-file) "pbp") (_msvc-possibly-setup-midtech-header-defaults "PlatformBuilder"))
   ((string= (file-name-extension proj-file) "dsp") (_msvc-possibly-setup-midtech-header-defaults "msvc6"))
   (t                                               (_msvc-possibly-setup-midtech-header-defaults "msvc6")))

  (message "Loaded %s" msvc-current-project))


(defun msvc-add-file-to-project-data (fname working-directory)
  (when (not (string= system-type "windows-nt"))
    ;; translate MS-DOS path seperators into Unix path seperators
    (let ((tmp fname) (i 0) (name-length  (length fname)))
      (while (< i name-length)
        (when (string= (substring tmp i (1+ i)) "\\")
          (store-substring tmp i "/"))
        (setq i (1+ i)))
      (setq fname tmp)))

  ;; (message "raw to add: %s" fullname)

  (let* ((fullname (expand-file-name fname working-directory)))
    (_msvc-filelookup-add-to-tree fullname)))


(defun _msvc-have-current-project ()
  "Has the user loaded a project?"
  (or (equal msvc-current-compilation-system 'gmake)
      (equal msvc-current-compilation-system 'rtags-ide)
      (equal msvc-current-compilation-system 'other)
      (and (boundp 'msvc-current-project)
                       (boundp 'msvc-current-config))))
  
(defun msvc-compile-current-project ()
  "Compile the current project.

The current project is set with either 'msvc-load-project'

Note- msdev.exe must be in your PATH for MSVC projects."

  (interactive)

  (when (not (_msvc-have-current-project))
    (error "Current project is not set.  Use M-x msvc-load-project"))

  (let (cmd (buffer
             (get-buffer-create (generate-new-buffer-name
                                 "*tmp-mscv-comp-dir*"))))
    (set-buffer buffer)
    (setq default-directory msvc-project-directory)
    (cond
     ((null msvc-current-compilation-system)
      (setq cmd ""))
     ((equal msvc-current-compilation-system 'msvc)
      (setq cmd (format "%s \"%s\" /make \"%s\" %s"
                        msvc-current-ide
                        msvc-current-project
                        msvc-current-config
                        (cond (msvc-evc-ceconfig
                               (format " /CEConfig=\"%s\" " msvc-evc-ceconfig))
                              (t "")))))
     ((equal msvc-current-compilation-system 'iar)
      (setq cmd (format "%s \"%s\" -make %s"
                        msvc-iarbuild-exe
                        msvc-current-project
                        msvc-current-config)))
     ((equal msvc-current-compilation-system 'uvision)
      (setq cmd (format "%s --prj=\"%s\"  --cfg=%s -l"
                        msvc-uvbuild-exe
                        (msvc-convert-unix-file-name-to-windows msvc-current-project)
                        msvc-current-config)))
     ((equal msvc-current-compilation-system 'rtags-ide)
      ;;
      ;; We need to
      ;;   1) look at the compile_commands.json.  If it's a symlink
      ;;      then we'll want to compile in the directory pointed to.
      ;;   2) Figure out if we have a ninja or make based build. 
      (message "Don't know how to compile with rtags-ide yet")
      
      (setq cmd (read-from-minibuffer "Compile project command: "
                                 compile-command nil nil
                                 '(compile-history . 1))))
     (t
      (setq cmd (read-from-minibuffer "Compile project command: "
                                 compile-command nil nil
                                 '(compile-history . 1)))))
    (save-some-buffers)
    (message "running %s" cmd)
    (compile cmd)
    (kill-buffer buffer)))



(defun msvc-find-file (file)
  "Switch to a buffer displaying a file from the project file list"

  (interactive
   (list
    (completing-read "Find MS Visual C++ File: "
                     (cond
                      ((not (_msvc-have-current-project))
                       (error "Current project is not set.  Use M-x msvc-load-project")
                       nil)
                      (t (_msvc-filelookup-files-alist)))
                     nil     ; predicate
                     t       ; require-match
                     nil     ; init
                     nil     ; hist
                     nil)))  ; def


  (let* ((stored         (cdr (assoc file (_msvc-filelookup-files-alist))))
         (extension      (downcase (_msvc_file-name-extension file)))
         (basename       (file-name-sans-extension (file-name-nondirectory file)))
         (full-file-name (cond (stored
                                stored)
                               ((member extension msvc-c-source-extensions)
                                (_msvc-filelookup-source basename))
                               ((member extension msvc-c-header-extensions)
                                (_msvc-filelookup-header basename)))))
    ;;(message "stored is %s" stored)
    (message "%s" full-file-name)
    (find-file full-file-name)))



(defun _msvc-next-in-list (fname files)
  (let ((result
         (cond ((and (string= (nth 0 files) fname) (nth 1 files))   (nth 1 files))
               ((and (string= (nth 0 files) fname) (nth 2 files))   (nth 2 files))
               ((and (string= (nth 1 files) fname) (nth 2 files))   (nth 2 files))
               ((and (string= (nth 1 files) fname) (nth 0 files))   (nth 0 files))
               ((and (string= (nth 2 files) fname) (nth 0 files))   (nth 0 files))
               ((and (string= (nth 2 files) fname) (nth 1 files))   (nth 1 files))
               (t                                                   nil))))
    result))


(defun msvc-find-other-file ()
  "If we are currently visiting a header file, try to find
the source file and viceversa.  If we don't have a current
project, or if the other file isn't found in the
msvc-project-file-tree variable then call ff-find-other-file.
"

  (interactive)

  (cond
   ((eq msvc-current-compilation-system 'rtags-ide)
    (let* ((full_base (file-name-sans-extension (buffer-file-name)))
           (base      (file-name-nondirectory full_base))
           (ext       (file-name-extension (buffer-file-name)))
           (look-for  (concat base (cond ((string-equal ext "cpp") ".h")
                                         (t                        ".cpp")))))
      (rtags-find-file nil look-for)))
   (t
    (let* ((buf-fname (buffer-file-name))
           (key       (_msvc-file-name-key buf-fname))
           (files     (_msvc-filelookup-get-item key))
           (file      (_msvc-next-in-list buf-fname files)))
      (cond (file   (find-file file))
            (t      (ff-find-other-file)))))))



(defun msvc_helper-python-like-format (fmt rep)
  "Format a string using a python-like string replacement

   Note- Currently the only supported format character is 's',
   the string character.
  "
  (save-excursion
    (let  (result (tmp-buffer (generate-new-buffer "*FMT-TEMP-BUFF*")))
      (set-buffer tmp-buffer)
      (insert fmt)
      (while rep
        (let ((item (car rep)))
          (let
              ((FROM-STRING (format "%%(%s)s" (car item)))
               (TO-STRING   (cadr item)))

            (goto-char (point-min))
            (while (search-forward FROM-STRING nil t)
              (replace-match TO-STRING nil t))))
        (setq rep (cdr rep)))
      (setq result (buffer-substring (point-min) (point-max)))
      (kill-buffer tmp-buffer)
      result)))


(defun msvc-doxygen (proj-name output-dir)
  "Run doxygen on all the C/C++ source files in the current project"

  (interactive "sProject: \nFOutput Directory: "
               )

  (set-buffer (get-buffer-create "*doxygen-file-list*"))
  (delete-region (point-min) (point-max))
  (insert (msvc_helper-python-like-format msvc-doxygen-template
                                          (list (list "proj_name"    proj-name)
                                                (list "output_dir"   output-dir)
                                                (list "dot_path"     msvc-dot-path)
                                                (list "hhc_location"
                                                      (if (boundp 'msvc-hhc-path)
                                                          msvc-hhc-path
                                                        ""))
                                                (list "warn_logfile"
                                                      (if (string= system-type "windows-nt")
                                                          "c:\\temp\\doxyout\\doxy_warnings.txt"
                                                        "/tmp/doxy_warnings.txt")
                                                      ))))


  (let
      ((files (_msvc-filelookup-filetuples-list))
       (doxyfile (if (string= system-type "windows-nt") "c:\\temp\\.tmp_doxyfile" "/tmp/.tmp_doxyfile")))
    (goto-char (point-max))
    (insert (format "\nINPUT = \n"))
    (mapcar (lambda (x)
              (insert (format "# looking at %s\n" x))
              (cond ((or (not (cadr x))
                         (and (not (string-match "^\/usr\/lib\/gcc-lib\/" (cadr x)))
                              (not (string-match "^\/usr\/include\/" (cadr x)))
                              (not (string-match "^\/usr\/local\/include\/" (cadr x)))
                              (not (string-match "^\/usr\/local\/embedded-arm-cross\/" (cadr x)))))
                     (cond ((car x)
                            (insert (format "INPUT += %s\n" (car x)))))
                     (cond ((cadr x)
                            (insert (format "INPUT += %s\n" (cadr x))))))))
            files)
    (goto-char (point-min))
    (while (search-forward "/cygdrive/z/" nil t)
      (replace-match "z:/" nil t))
    (goto-char (point-min))
    (while (search-forward "/cygdrive/e/" nil t)
      (replace-match "e:/" nil t))
    (goto-char (point-min))
    (while (search-forward "/cygdrive/d/" nil t)
      (replace-match "d:/" nil t))
    (write-region (point-min) (point-max) doxyfile)
    (start-process "*doxyproc*" "*doxygen-output*" msvc-doxygen-exe doxyfile)
    )
)



(defun msvc-cccc (output-dir)
  "Run CCCC on all the C/C++ source files in the current project"

  (interactive "FOutput Directory: ")

  (set-buffer (get-buffer-create "*cccc-file-list*"))
  (delete-region (point-min) (point-max))

  (let
      ((files (_msvc-filelookup-filetuples-list))
       (file-list "")
       proc
      )
    (goto-char (point-max))
    (mapcar (lambda (x)
              (cond ((and (cadr x)
                          (not (string-match "libkml-1.1.0" (cadr x)))
                          (not (string-match ".*/\.moc_" (cadr x)))
                          (not (string-match "README.txt.h" (cadr x)))
                          (not (string-match "^\/usr\/lib\/gcc-lib\/" (cadr x)))
                          (not (string-match "^\/usr\/include\/" (cadr x)))
                          (not (string-match "^\/usr\/local\/include\/" (cadr x)))
                          (not (string-match "^\/usr\/local\/embedded-arm-cross\/" (cadr x))))
                     (cond ((car x) (insert (format "%s\n" (car x)))))
                     (cond ((cadr x) (insert (format "%s\n" (cadr x))))))))
            files)
    (goto-char (point-min))
    (while (search-forward "/cygdrive/z/" nil t)
      (replace-match "z:/" nil t))
    (goto-char (point-min))
    (while (search-forward "/cygdrive/e/" nil t)
      (replace-match "e:/" nil t))
    (goto-char (point-min))
    (while (search-forward "/cygdrive/d/" nil t)
      (replace-match "d:/" nil t))
    (setq file-list (buffer-string))
    (setq proc
          (start-process "*cccc-run*" "*cccc-run-output*" msvc-cccc-exe
                        (format "--outdir=%s" output-dir)
                         "-"))
    (process-send-string proc file-list)
    (process-send-eof proc)
    (message "sent %s" file-list)
    )
)


(defun msvc-save-file-list (file-name)
  "Save the list of project source files into a file"

  (interactive "F")

  (save-excursion
  (let (proc
        (tmp-buffer-name "*create-file-list-tmp-buffer*")
        (headers-str "")
        (sources-str ""))

    (set-buffer (get-buffer-create tmp-buffer-name))
    (setq default-directory msvc-project-directory)

  (delete-region (point-min) (point-max))
  (goto-char (point-max))

    (mapc (lambda (x)
            (unless (file-exists-p x)
              (message "couldn't find %s" x))
            (message "adding %s" x)
            (cond ((string-match ".*\\.[hH]\\>" x)
                   (setq headers-str (format "%s\n%s" x headers-str)))
                  (t
                   (setq sources-str (format "%s\n%s" x sources-str)))))
          msvc-project-file-list)
    (insert (format "%s%s" headers-str sources-str))
    (goto-char (point-min))
    (while (search-forward "\\" nil t)
      (replace-match "/" nil t))
    (write-file file-name))))


(defun msvc-create-gtags ()
  "Create a gtags file on all the C/C++ source files in the current project"

  (interactive)

  ;; (setq default-directory msvc-project-directory)
  (setq default-directory "/home/cts/BM/CodeBase/")

  (let (proc
        (file-name (make-temp-name
                    (expand-file-name ".tmp_gtags"
                                      (cond
                                       ((boundp 'temporary-file-directory)
                                        temporary-file-directory)
                                       (t
                                        (temp-directory)))))))
    (msvc-save-file-list file-name)

    (setq proc (start-process "*create-gtags-file-list-proc*"
                              "*create-gtags-output*"
                              msvc-gtags-exe
                              "--file" file-name
                              ; (expand-file-name "~")))
                              (expand-file-name "/home/cts/BM/CodeBase/")))
;                                          "."
;                                          msvc-project-directory )))
    (message "started process %s" proc)
    )
  )


(defun msvc-create-ebrowse-file ()
  "Create an ebrowse file on all the C/C++ source files in the current project"

  (interactive)

  (setq default-directory msvc-project-directory)

  (let (proc
        (file-name (make-temp-name
                    (expand-file-name ".tmp_createebrowse"
                                      (cond
                                       ((boundp 'temporary-file-directory)
                                        temporary-file-directory)
                                       (t
                                        (temp-directory)))))))
    (msvc-save-file-list file-name)

    (setq proc (start-process "*create-ebrowse-file-list-proc*"
                              "*create-ebrowse-output*"
                              msvc-ebrowse-exe "--very-verbose"
                              "--files" file-name
                              "--output" (expand-file-name
                                          "BROWSE"
                                          msvc-project-directory )))
    (message "started process %s" proc)
    )
  )



(defun msvc-create-ebrowse-file.old ()
  "Create an ebrowse file on all the C/C++ source files in the current project"

  (interactive)

  (set-buffer (get-buffer-create "*create-ebrowse-tmp-buffer*"))
  (setq default-directory msvc-project-directory)

  (delete-region (point-min) (point-max))
  (goto-char (point-max))

  (let (proc
        (headers-str "")
        (sources-str "")
        (file-name (make-temp-name
                    (expand-file-name ".tmp_createebrowse"
                                      (cond
                                       ((boundp 'temporary-file-directory)
                                        temporary-file-directory)
                                       (t
                                        (temp-directory)))))))
    (mapc (lambda (x)
            (unless (file-exists-p x)
              (message "couldn't find %s" x))
             (message "adding %s" x)
            (cond ((string-match ".*\\.[hH]\\>" x)
                   (setq headers-str (format "%s\n%s" x headers-str)))
                  (t
                   (setq sources-str (format "%s\n%s" x sources-str)))))
          msvc-project-file-list)
    (insert (format "%s%s" headers-str sources-str))
    (goto-char (point-min))
    (while (search-forward "\\" nil t)
      (replace-match "/" nil t))
    (write-file file-name)
    (setq proc (start-process "*create-ebrowse-file-list-proc*"
                              "*create-ebrowse-output*"
                              msvc-ebrowse-exe "--very-verbose"
                              "--files" file-name
                              "--output" (expand-file-name
                                          "BROWSE"
                                          msvc-project-directory )))
    (message "started process %s" proc)
    )
  )


(defun _msvc-possibly-setup-midtech-header-defaults (type)
  (if (boundp 'midtech-header-default-compiler-type) (midtech-set-comment-defaults type)))


(defun msvc-tags-find-declaration (&optional prefix)
  "View declaration of member at point"
  (interactive)
  (message "prefix is %s" prefix)
  (cond ((eq msvc-current-compilation-system 'rtags-ide)
         (rtags-find-symbol-at-point prefix))
        (t
         (cond (prefix (ebrowse-tags-find-declaration-other-window))
               (t (ebrowse-tags-find-declaration))))))

(defun msvc-tags-find-definition (&optional prefix)
  "View definition of member at point"
  (interactive)
  (message "prefix is %s" prefix)
  (cond ((eq msvc-current-compilation-system 'rtags-ide)
         (rtags-find-symbol-at-point prefix))
        (t
         (cond (prefix (ebrowse-tags-find-definition-other-window))
               (t (ebrowse-tags-find-definition))))))


;;; I seem to be missing a couple of functions that the AVL tree wants.
(when (not (functionp 'elib-stack-push))
       (defun elib-stack-push (a b) (stack-push a b)))

(when (not (functionp 'elib-stack-pop))
       (defun elib-stack-pop (a) (stack-pop a)))

(when (not (functionp 'elib-stack-create))
       (defun elib-stack-create () (stack-create)))


;;; Older versions of emacs don't have the string-prefix-p function
(when (not (functionp 'string-prefix-p))
  (defun string-prefix-p (str1 str2 &optional ignore-case)
    (let* ((end-pos (length str1)))
      (equal t (compare-strings str1 0 end-pos
                       str2 0 end-pos ignore-case)))))
