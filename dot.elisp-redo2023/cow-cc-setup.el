;;; Sets of a few things for editing C/C++ code.
;;;


(message "Loading settings for cc-mode.")

(require 'cow-style)

(add-hook 'c-mode-common-hook
          #'(lambda ()
              (c-set-style cow-cc-coding-standard)))

(provide 'cow-cc-setup)
