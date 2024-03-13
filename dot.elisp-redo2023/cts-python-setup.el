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
(require 'cts-ac-common)



(provide 'cts-python-setup)
