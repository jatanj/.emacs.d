(require 'spaceline-config)

(setq powerline-default-separator nil)
(setq spaceline-workspace-numbers-unicode t)
;; (setq spaceline-window-numbers-unicode t)

(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-anzu-off)

(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)

(provide 'config-spaceline)
