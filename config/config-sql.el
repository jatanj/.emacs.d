(use-package sql-indent
  :ensure t
  :init
  (add-hook 'sql-mode-hook
            (lambda ()
              (electric-indent-local-mode -1)
              (sqlind-minor-mode))))

(provide 'config-sql)
