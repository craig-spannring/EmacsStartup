;; -*- lexical-binding: t; -*-

;;; Setup to use GUD.
;;;
;;; See https://emacs-lsp.github.io/dap-mode/page/configuration/
;;; for a possible way of fleshing this out that might look like this
;;;      (use-package lsp-mode :ensure t)
;;;      (use-package dap-mode :ensure t)
;;;      (use-package lsp-ui :ensure t)
;;;      
;;;      ;; (use-package lsp-mode
;;;      ;;   :hook ((c++-mode . lsp))
;;;      ;;   :commands lsp)
;;;      
;;;      (use-package dap-mode
;;;        :after lsp-mode
;;;        :config
;;;        (dap-auto-configure-mode))
;;;      
;;;      ;; (use-package dap-lldb
;;;      ;;   :after lsp-mode)
;;;      (require 'dap-lldb)
;;;      
;;;      (setq dap-lldb-debug-program '("/opt/local/bin/lldb-dap-mp-21"))
;;;      
;;;      (use-package lsp-ui
;;;        :commands lsp-ui-mode)
;;;      
;;;      ;; (use-package company
;;;      ;;   :hook (after-init . global-company-mode))
;;;      

(require 'cow-cpp-setup)
(require 'cc-mode)

(defcustom cow-preferred-debug 'realgud
  "Select which Emacs package to use for debuggingn. 
Note- Changes won't take effect until you restart emacs."
  :type '(choice (const realgud)  (const gud) (const lldb-dap))
  :group 'cow-emacs-conf)


(defun _cow-find-dap-lldb-debug-program()
  "Try to find the lldb-dap executable."
  (cond ((executable-find "lldb-dap")       (executable-find "lldb-dap"))
        ((executable-find "lldb-dap-mp-21") (executable-find "lldb-dap-mp-21"))
        (t (let ((possible-paths '("/opt/local/bin/lldb-dap-mp-21"
                                   "/usr/local/bin/lldb-dap"
                                   "/usr/bin/lldb-dap")))
             (catch 'found
               (dolist (path possible-paths)
                 (when (file-executable-p path)
                   (throw 'found path))))))))

(cond ((equal cow-preferred-debug 'realgud)
       (message "Setting up for realgud")
       (use-package realgud      :ensure t)
       (use-package realgud-lldb :ensure t)

       (require 'cow-keys-realgud))
      ((equal cow-preferred-debug 'lldb-dap)
       (message "Setting up for lldb-dap")

       (require 'dap-lldb)

        (dap-mode 1)         
        (dap-ui-mode 1)      
        (dap-tooltip-mode 1) 
        (tooltip-mode 1)     
        (dap-ui-controls-mode 1)

       (if (not (_cow-find-dap-lldb-debug-program))
           (message (concat "Warning- Could not find lldb-dap executable.  "
                            "Please install it and set `dap-lldb-debug-program` accordingly."))
         (setq dap-lldb-debug-program (_cow-find-dap-lldb-debug-program)))
       
      
              (require 'cow-keys-dap-lldb))
      ((equal cow-preferred-debug 'gud)
       (message "Setting up for gud")
       (require 'cow-keys-gud))
      (nil
       (message "Unknown debugger preference %s.")))


;; (use-package dap-mode :ensure t)

;; (require 'dap-cpptools)
;; (require 'dap-codelldb)
;; (require 'dap-gdb-lldb)

;; (dap-codelldb-setup)

;; (use-package dap-mode
;;   :after lsp-mode
;;   :config
;;   (dap-auto-configure-mode))
;; 


(provide 'cow-debugger-setup)
