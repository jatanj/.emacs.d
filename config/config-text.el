(defun config/text-mode-init ()
  (electric-indent-local-mode -1))

(add-hook 'text-mode-hook #'config/text-mode-init)

(provide 'config-text)
