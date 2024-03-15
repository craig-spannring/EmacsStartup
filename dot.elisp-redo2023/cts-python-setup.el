;;; This sets up a python IDE
;;;
;;;                                 LSP
;;; Two choices for client
;;; * Eglot
;;;     
;;; * LspMode
;;;
;;; N choices for python  server
;;; * Jedi
;;; * EPC 



(message "Loading python support.")

(require 'cts-package-helpers)
(require 'cts-complete-common)

(defcustom cts-python-which-support 'cow
  "Select which style of IDE to use for Python.
Note- Changes won't take effect until you restart emacs."
  :type '(choice
          (const cow)      ; Craig' own way
          (const enzu)     ; See https://enzuru.medium.com/helpful-emacs-python-mode-hooks-especially-for-type-hinting-c4b70b9b2216
          (const mattduck) ; See https://www.mattduck.com/lsp-python-getting-started.html
          (const none))
  :group 'cts-emacs-conf)


(cond
 ((equal cts-python-which-support 'cow)      (require 'cts-python-setup-cow))
 ((equal cts-python-which-support 'enzu)     (message "Enzu python support package is not supported yet."))
 ((equal cts-python-which-support 'mattduck) (require 'cts-python-setup-mattduck))
 (t                                          (message "Unknown python support package.")))

(provide 'cts-python-setup)
