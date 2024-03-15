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
  :group 'cts-setup-python)

(cond
 ((equal cts-python-which-support 'cow)
  (require 'cts-python-setup-cow)
  (message "here4")
  )
 ((equal cts-python-which-support 'enzu)
  (message "Enzu python support package is not supported yet."))
 ((equal cts-python-which-support 'mattduck)
  (message "Setting up mattduck python support.")
  (use-package lsp-mode
               :config
               (setq lsp-idle-delay 0.5
                     lsp-enable-symbol-highlighting t
                     lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
                     lsp-pyls-plugins-flake8-enabled t)
               (lsp-register-custom-settings
                '(("pyls.plugins.pyls_mypy.enabled" t t)
                  ("pyls.plugins.pyls_mypy.live_mode" nil t)
                  ("pyls.plugins.pyls_black.enabled" t t)
                  ("pyls.plugins.pyls_isort.enabled" t t)

                  ;; Disable these as they're duplicated by flake8
                  ("pyls.plugins.pycodestyle.enabled" nil t)
                  ("pyls.plugins.mccabe.enabled" nil t)
                  ("pyls.plugins.pyflakes.enabled" nil t)))
               :hook
               ((python-mode . lsp)
                (lsp-mode . lsp-enable-which-key-integration))
               :bind (:map evil-normal-state-map
                           ("gh" . lsp-describe-thing-at-point)
                           :map md/leader-map
                           ("Ff" . lsp-format-buffer)
                           ("FR" . lsp-rename)))
  (use-package lsp-ui
               :config (setq lsp-ui-sideline-show-hover t
                             lsp-ui-sideline-delay 0.5
                             lsp-ui-doc-delay 5
                             lsp-ui-sideline-ignore-duplicates t
                             lsp-ui-doc-position 'bottom
                             lsp-ui-doc-alignment 'frame
                             lsp-ui-doc-header nil
                             lsp-ui-doc-include-signature t
                             lsp-ui-doc-use-childframe t)
               :commands lsp-ui-mode
               :bind (:map evil-normal-state-map
                           ("gd" . lsp-ui-peek-find-definitions)
                           ("gr" . lsp-ui-peek-find-references)
                           :map md/leader-map
                           ("Ni" . lsp-ui-imenu)))
  ;; (use-package pyvenv
  ;;              :demand t
  ;;              :config
  ;;              (setq pyvenv-workon "emacs") ; Default venv
  ;;              (pyvenv-tracking-mode 1))    ; Automatically use pyvenv-workon via dir-locals
  )
 (t
  (message "Unknown python support package.")))

(provide 'cts-python-setup)
