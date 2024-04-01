;; Some common definitions for LSP 
;;
;; 

(defcustom cow-which-lsp-package 'use-lsp  ;; other choice is 'use-eglot  
  "Which type of python support do we want to use?
lsp-mode seems to be the more powerful of the two modes. 

                                eglot

eglot key bindings 
   s-l g a      xref-find-apropos
   C-M-.        xref-find-apropos
   C-M-,        xref-go-forward
   C-x 4 .      xref-find-definitions-other-window
   C-x 5 .	xref-find-definitions-other-frame
   C-x 5 .      xref-find-definitions-other-frame
   M-.          xref-find-definitions
   M-?          xref-find-references
   M-,          xref-go-back
   M-,          xref-pop-marker-stack


                               lsp-mode
lsp-mode seems to have more features than eglot.  

Note- The default keyboard layout on a US 104 keyboard doesn't
have a super-key.  You might need to map one of the existing
keys (e.g. right-Windows key) to be the super key.

Standard lsp-mode keybindings:
   <mouse-3>    lsp-mouse-click
   s-l G g      lsp-ui-peek-find-definitions
   s-l G i      lsp-ui-peek-find-implementation
   s-l G r      lsp-ui-peek-find-references
   s-l G s      lsp-ui-peek-find-workspace-symbol
   s-l a a      lsp-execute-code-action
   s-l a h      lsp-document-highlight
   s-l r o      lsp-organize-imports
   s-l r r      lsp-rename
   s-l h g      lsp-ui-doc-glance
   s-l h h      lsp-describe-thing-at-point
   s-l h s      lsp-signature-activate
   s-l g d      lsp-find-declaration
   s-l g g      lsp-find-definition
   s-l g i      lsp-find-implementation
   s-l g r      lsp-find-references
   s-l g t      lsp-find-type-definition
   s-l T D      lsp-modeline-diagnostics-mode
   s-l T L      lsp-toggle-trace-io
   s-l T S      lsp-ui-sideline-mode
   s-l T a      lsp-modeline-code-actions-mode
   s-l T b      lsp-headerline-breadcrumb-mode
   s-l T d      lsp-ui-doc-mode
   s-l T f      lsp-toggle-on-type-formatting
   s-l T h      lsp-toggle-symbol-highlight
   s-l T s      lsp-toggle-signature-auto-activate
   s-l F a      lsp-workspace-folders-add
   s-l F b      lsp-workspace-blacklist-remove
   s-l F r      lsp-workspace-folders-remove
   s-l = =      lsp-format-buffer
   s-l = r      lsp-format-region
   s-l w D      lsp-disconnect
   s-l w d      lsp-describe-session
   s-l w q      lsp-workspace-shutdown
   s-l w r      lsp-workspace-restart

   s-l g a      xref-find-apropos
   M-,          xref-pop-marker-stack
   M-.          xref-find-definitions
   M-?          xref-find-references
   C-M-.        xref-find-apropos
   C-x 4 .      xref-find-definitions-other-window
   C-x 5 .      xref-find-definitions-other-frame
"
  :type '(choice
          (const use-eglot) ; see https://www.gnu.org/software/emacs/manual/html_mono/eglot.html
          (const use-lsp))  ; 
  :group 'cow-emacs-conf)

(cond
 ((equal cow-which-lsp-package 'use-lsp)
  (use-package lsp-mode :ensure t)
  (use-package lsp-ui   :ensure t))
 ((equal cow-which-lsp-package 'use-eglot)
  (use-package eglot))
 (t
  (message "Warning: cow-which-lsp-package's value is not recognized.")
  (sleep-for 2.0)))

(provide 'cow-lsp-common)
