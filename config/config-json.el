(use-package json-mode
  :straight t
  :init
  (defun config/json-mode-init ()
    (setq-local js-indent-level 2))
  (add-hook 'json-mode-hook #'config/json-mode-init))

(provide 'config-json)
