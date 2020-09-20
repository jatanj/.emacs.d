(use-package doom-themes
  :straight t
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic nil)
  :config
  (advice-add 'doom-themes--neotree-setup :after (lambda (&rest _) (hl-line-mode -1)))
  (doom-themes-neotree-config)
  (setq doom-neotree-enable-file-icons t))

(use-package doom-modeline
  :straight t
  :init
  (require 'doom-modeline)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-height 30)
  (setq doom-modeline-icon t)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-def-segment evil-state
    (when (bound-and-true-p evil-local-mode)
      (evil-state-property evil-state :tag t)))
  (setq evil-normal-state-tag (propertize " [N] " 'face '((:foreground "#f8f8f8" :background "#4f3598")))
        evil-emacs-state-tag (propertize " [E] " 'face  `(((:foreground "#f8f8f8" :background ,(get 'custom-theme-color-bg2 'saved-value)))))
        evil-insert-state-tag (propertize " [I] " 'face '((:foreground "#f8f8f8" :background "#7eaefd")))
        evil-motion-state-tag (propertize " [M] " 'face '((:foreground "#f8f8f8" :background "#e6987a")))
        evil-visual-state-tag (propertize " [V] " 'face '((:foreground "#f8f8f8" :background "#e6987a")))
        evil-operator-state-tag (propertize " [O] " 'face '((:foreground "#f8f8f8" :background "#4f3598"))))
  (doom-modeline-def-modeline 'main
    '(bar window-number " " evil-state " " workspace-name matches buffer-info remote-host parrot selection-info checker)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes word-count input-method indent-info buffer-position buffer-encoding process vcs "  "))
  (doom-modeline-mode 1)
  :config
  (defun doom-modeline-refresh ()
    (interactive)
    (doom-modeline-refresh-bars)
    (doom-modeline-refresh-font-width-cache))
  (general-define-key
   :prefix leader-key
   :keymaps 'doom-modeline-mode-map
   "d r" 'doom-modeline-refresh))

(provide 'config-doom)
