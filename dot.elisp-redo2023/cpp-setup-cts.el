;;; This sets up a C++ IDE
;;;

(message "Loading C++ support.")

(require 'package-helpers-cts)
(require 'complete-common-cts)

(global-set-key [f9 ?f] 'msvc-find-file)
(global-set-key [f9 ?c] 'msvc-compile-current-project)
(global-set-key [f9 f9] 'next-error)
(global-set-key [f5 ?c] 'compile)

(provide 'cpp-setup-cts)
