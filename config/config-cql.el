(use-package cql-mode
  :ensure t
  :mode (("\\.cql\\'" . cql-mode))
  :init
  (add-hook 'cql-mode-hook #'sqlind-minor-mode))

(provide 'config-cql)
