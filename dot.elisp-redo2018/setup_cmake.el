
(install-and-require-packages '(cmake-mode
				json
				find-file
				levenshtein
				cl-lib
				seq
				s
				dash
				rtags
				company
				company-rtags
				flycheck
				))

(rtags-enable-standard-keybindings)

(autoload 'cmake-mode "cmake-mode")
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

