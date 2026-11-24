;; -*- lexical-binding: t; -*-
;;
;;; Sets of a few things for editing C/C++ code.
;;;


(message "Loading settings for cc-mode.")

(require 'cow-style)

(add-hook 'c-mode-common-hook
          #'(lambda ()
              (c-set-style cow-cc-coding-style)))


(defun _cow-c-header--insert-guard-position ()
  "Return the insertion point for the include guard.

If a comment begins on line 1, insert after it. Otherwise insert at the
beginning of the file."
  (save-excursion
    (goto-char (point-min))
    (let* ((line1-start (point))
           (line1-end (min (point-max) (line-end-position)))
           (line1 (buffer-substring-no-properties line1-start line1-end)))
      (cond
       ;; Single-line comment on line 1
       ((string-match-p "^[[:space:]]*//" line1)
        (end-of-line)
        (point))

       ;; Block comment starting on line 1
       ((string-match-p "^[[:space:]]*/\\*" line1)
        (let ((pos (search-forward "*/" nil t)))
          (unless pos
            (user-error "Unterminated block comment at the start of the file"))
          (point)))

       ;; No comment on line 1: insert at top
       (t
        (point-min))))))

(defun cow-insert-dot-h-guard ()
  "Insert the multi-inclusion guard for a C/C++ header file.

If the file starts with a leading comment on line 1, insert the guard after
that comment; otherwise insert the guard at the top of the file."

  (interactive)
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))

  (let* ((full-path (expand-file-name (buffer-file-name)))
         (file-name (file-name-nondirectory full-path))
         (ext (file-name-extension file-name))
         (valid-exts '("h" "hpp" "hxx")))
    (unless (member ext valid-exts)
      (user-error "Current file is not a .h, .hpp, or .hxx header"))

    (let* ((base (file-name-sans-extension file-name))
           (base-safe (upcase (replace-regexp-in-string "[^A-Za-z0-9_]" "_" base)))
           (ext-safe (downcase (replace-regexp-in-string "[^A-Za-z0-9_]" "_" ext)))
           (md5sum (md5 full-path))
           (guard-name (format "%s_%s_%s" base-safe md5sum ext-safe))
           (guard-text (format "#ifndef %s\n#define %s\n\n" guard-name guard-name))
           (endif-text "\n#endif\n")
           (insert-at (_cow-c-header--insert-guard-position)))

      (save-excursion
        (goto-char insert-at)
        ;; If insertion is after a leading comment, make sure we start a new line
        ;; before the guard.
        (when (and (> insert-at (point-min))
                   (not (bolp))
                   (not (= (char-before insert-at) ?\n)))
          (insert "\n"))
        (insert guard-text)

        ;; Append closing guard at end of file
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert endif-text)))))

(provide 'cow-cc-setup)
