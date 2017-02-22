(use-package rainbow-mode
  :ensure t
  :config
  (dolist (mode '(css-mode
                  scss-mode))
    (add-hook (intern (format "%s-hook" mode)) #'rainbow-mode)))
(provide 'config-rainbow-mode)
