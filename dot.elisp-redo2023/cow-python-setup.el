;;; This sets up a python IDE
;;;
;;;                                 LSP
;;; Two choices for client
;;; * Eglot     
;;; * LspMode
;;;
;;; N choices for python  server
;;; * Jedi
;;; * EPC
;;;
;;;
;;;                         Prerequisites
;;; pylsp
;;;     sudo apt install python3-pylsp
;;; 


(message "Loading python support.")

(require 'cow-package-helpers)
(require 'cow-complete-common)

(defcustom cow-python-which-support 'cow
  "Select which style of IDE to use for Python.
Note- Changes won't take effect until you restart emacs."
  :type '(choice
          (const cow)      ; Craig' own way
          (const enzu)     ; See https://enzuru.medium.com/helpful-emacs-python-mode-hooks-especially-for-type-hinting-c4b70b9b2216
          (const mattduck) ; See https://www.mattduck.com/lsp-python-getting-started.html
          (const none))
  :group 'cow-emacs-conf)

(message "Using %s for python support" cow-python-which-support)

(cond
 ((equal cow-python-which-support 'cow)      (require 'cow-python-setup-cow))
 ((equal cow-python-which-support 'enzu)     (message "Enzu python support package is not supported yet."))
 ((equal cow-python-which-support 'mattduck) (require 'cow-python-setup-mattduck))
 (t                                          (message "Unknown python support package.")))

(provide 'cow-python-setup)
