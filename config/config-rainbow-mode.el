(use-package rainbow-mode
  :ensure t
  :init
  (dolist (mode '(css-mode scss-mode))
    (add-hook (intern (format "%s-hook" mode)) #'rainbow-mode)))

(provide 'config-rainbow-mode)
