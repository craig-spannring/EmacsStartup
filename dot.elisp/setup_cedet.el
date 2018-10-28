
(when nil
  (message "Setting up support for ede-locate-setup")

  (setq ede-locate-setup-options '(ede-locate-global ede-locate-base))
  (load-file "~/.elisp/cedet-1.0pre6/common/cedet.el")
  (global-ede-mode 1)

  (semantic-load-enable-minimum-features)
  (semantic-load-enable-code-helpers)

  (require 'semantic-ia)

  ;; * This enables even more coding tools such as intellisense mode
  ;;   decoration mode, and stickyfunc mode (plus regular code helpers)
  (semantic-load-enable-gaudy-code-helpers)

  (require 'semanticdb-global)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

                                        ;(require 'semanticdb-ebrowse)
                                        ;(semanticdb-load-ebrowse-caches)

  (defun my-cedet-hook ()
    (local-set-key [(control return)] 'semantic-ia-complete-symbol)
    (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
    (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
    (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
  (add-hook 'c-mode-common-hook 'my-cedet-hook)


                                        ;(require 'info)
                                        ;     (setq Info-directory-list
                                        ;      (cons (expand-file-name "/home/cts/.elisp/cedet-1.0pre6/cogre/")
                                        ;            Info-directory-list))

  )