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

(require 'cmake-ide)
(cmake-ide-setup) 
