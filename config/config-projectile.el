(projectile-global-mode)

(setq projectile-indexing-method 'alien)
;; (setq projectile-enable-caching t)

(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories ".cache")

(setq projectile-keymap-prefix (kbd "C-k p"))

(provide 'config-projectile)
