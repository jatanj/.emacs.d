(require 'spaceline-config)

(setq powerline-default-separator nil)
(setq spaceline-workspace-numbers-unicode t)
;; (setq spaceline-window-numbers-unicode t)

(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-anzu-off)

(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

(add-to-list 'configure-frame-functions
  (lambda ()
    (set-face-attribute 'spaceline-evil-insert nil :background "#7eaefd")
    (set-face-attribute 'spaceline-evil-normal nil :background "#4f3598" :foreground "#ffffff")
    (set-face-attribute 'spaceline-evil-replace nil :background "#005154" :foreground "#ffffff")
    (set-face-attribute 'spaceline-evil-visual nil :background "#e6987a")
    (set-face-attribute 'spaceline-evil-emacs nil :background "#393d44" :foreground "#ffffff")))

(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)

(provide 'config-spaceline)
