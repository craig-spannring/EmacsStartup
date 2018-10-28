
(require 'xml) 

(load "msvc_functions")

(when (or (equal (system-name) "HYDRA")
          (equal (system-name) "SEVILLE"))
  (setq msvc-doxygen-exe "c:\\Program Files\\doxygen\\bin\\doxygen.exe"))

(condition-case err (require 'fdjsakfdsa) (error (message "foo")))

(when (string-equal emacs-version "21.3.1")
  (defun xml-parse-attlist (end)
    "Return the attribute-list that point is looking at.
The search for attributes end at the position END in the current buffer.
Leaves the point on the first non-blank character after the tag."
    (let ((attlist '())
          name)
      (skip-chars-forward " \t\n")
      (while (looking-at "\\([a-zA-Z_:][-a-zA-Z0-9._:]*\\)[ \t\n]*=[ \t\n]*")
        (set 'name (intern (match-string 1)))
        (goto-char (match-end 0))

        ;; Do we have a string between quotes (or double-quotes),
        ;;  or a simple word ?
        (unless (looking-at "\"\\([^\"]*\\)\"")
          (unless (looking-at "'\\([^']*\\)'")
            (error "XML: Attribute values must be given between quotes!")))

        ;; Each attribute must be unique within a given element
        (if (assoc name attlist)
            (error "XML: each attribute must be unique within an element"))
      
        (set 'attlist (append attlist
                              (list (cons name (match-string-no-properties 1)))))
        (goto-char (match-end 0))
        (skip-chars-forward " \t\n")
        (if (> (point) end)
            (error "XML: end of attribute list not found before end of region"))
        )
      attlist
      )))
