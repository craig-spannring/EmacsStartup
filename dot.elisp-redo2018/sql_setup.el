(install-and-require-packages '(sql-indent))

(add-hook 'sql-mode-hook
          #'sqlind-minor-mode)
