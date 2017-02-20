(use-package helm
  :config
  (require 'helm-config)

  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 25)

  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-split-window-in-side-p t)

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.25)))

  (setq helm-boring-buffer-regexp-list
        '("\\` "
          "\\*helm"
          "\\*helm-mode"
          "\\*Echo Area"
          "\\*tramp"
          "\\*Minibuf"
          "\\*Ibuffer\\*"
          "\\*tide-server\\*"
          "\\*fsharp-complete\\*"))

  (global-set-key (kbd "C-S-p") 'helm-M-x)
  (global-set-key (kbd "C-p") 'helm-buffers-list))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helm-ag
  :config
  (setq helm-ag-fuzzy-match t))

(provide 'config-helm)
