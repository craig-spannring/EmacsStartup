
(install-and-require-packages '(cmake-mode))

(autoload 'cmake-mode "cmake-mode")
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(require 'cmake-ide)
(cmake-ide-setup) 
