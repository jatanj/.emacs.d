(use-package sql-indent
  :straight t
  :init
  (add-hook 'sql-mode-hook
            (lambda ()
              (electric-indent-local-mode -1)
              (sqlind-minor-mode))))

(provide 'config-sql)
