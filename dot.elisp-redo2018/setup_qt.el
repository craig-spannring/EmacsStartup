;;;
;;; Setup mode for Qt programming.
;;;
;;; If needed, this installs qt-pro-mode and qml-mode.  It then adds
;;; entries to the auto-mode-alist for .pro, .pri, and .qml files
;;; 

(install-and-require-packages '(qt-pro-mode qml-mode))

(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))
(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))



(add-hook 'qml-mode-hook
          (lambda ()
            (setq-local comment-start "//")
            (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
            (setq-local comment-end "")
            (setq-local comment-end-skip nil)
            (setq-local comment-continue nil)))
