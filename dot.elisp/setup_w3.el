
;; (setq load-path (cons "~/.elisp/w3-4.0pre.47/lisp" load-path))

;; (condition-case nil
;;     (progn (require 'w3-auto))
;;   (error nil))


;; (setq w3m-init-file "~/.elisp/emacs-w3m")

;; (condition-case nil
;;      (progn (require 'w3m-load))
;;    (error nil))

;; (condition-case nil
;;      (progn (require 'mime-w3m))
;;    (error nil))



(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
;; (global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)


(provide 'setup_w3)
