(require 'projectile)

(projectile-global-mode)

(setq projectile-indexing-method 'alien)
;; (setq projectile-enable-caching t)

(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories ".cache")

(define-key projectile-mode-map projectile-keymap-prefix nil)
(define-key projectile-mode-map (kbd "C-l p") #'projectile-command-map)

(provide 'config-projectile)
