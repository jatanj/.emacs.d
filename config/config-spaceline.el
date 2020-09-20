(use-package spaceline
  :straight t
  :config
  (require 'spaceline-config)
  (spaceline-define-segment buffer-id
    (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      (powerline-buffer-id)))
  (setq powerline-default-separator nil)
  (setq spaceline-workspace-numbers-unicode t)
  (setq spaceline-window-numbers-unicode nil)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-anzu-off)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode 1))

(provide 'config-spaceline)
