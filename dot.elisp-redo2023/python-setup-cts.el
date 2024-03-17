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

(require 'package-helpers-cts)
(require 'complete-common-cts)

(defcustom cts-python-which-support 'cow
  "Select which style of IDE to use for Python.
Note- Changes won't take effect until you restart emacs."
  :type '(choice
          (const cow)      ; Craig' own way
          (const enzu)     ; See https://enzuru.medium.com/helpful-emacs-python-mode-hooks-especially-for-type-hinting-c4b70b9b2216
          (const mattduck) ; See https://www.mattduck.com/lsp-python-getting-started.html
          (const none))
  :group 'cts-emacs-conf)

(message "Using %s for python support" cts-python-which-support)

(cond
 ((equal cts-python-which-support 'cow)      (require 'python-setup-cow-cts))
 ((equal cts-python-which-support 'enzu)     (message "Enzu python support package is not supported yet."))
 ((equal cts-python-which-support 'mattduck) (require 'python-setup-mattduck-cts))
 (t                                          (message "Unknown python support package.")))

(provide 'python-setup-cts)
