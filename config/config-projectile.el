(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'alien)
  ;; (setq projectile-enable-caching t)
  ;; (setq 'projectile-ignored-projects '())
  (define-key projectile-mode-map projectile-keymap-prefix nil)
  (define-key projectile-mode-map (kbd "C-l p") #'projectile-command-map))

(provide 'config-projectile)
