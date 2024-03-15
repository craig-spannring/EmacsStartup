;;; This sets up a C++ IDE
;;;

(message "Loading C++ support.")

(require 'cts-package-helpers)
(require 'cts-complete-common)

(global-set-key [f9 ?f] 'msvc-find-file)
(global-set-key [f9 ?c] 'msvc-compile-current-project)
(global-set-key [f9 f9] 'next-error)
(global-set-key [f5 ?c] 'compile)

(provide 'cts-cpp-setup)
