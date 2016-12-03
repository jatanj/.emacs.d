(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-l p"))

  :config
  (setq projectile-indexing-method 'alien)
  ;; (setq projectile-enable-caching t)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (projectile-global-mode))

(provide 'config-projectile)
