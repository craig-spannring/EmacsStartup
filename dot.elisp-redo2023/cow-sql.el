
(use-package sql-indent :ensure t)
(add-hook 'sql-mode-hook
          #'sqlind-minor-mode)

(provide 'cow-sql)
