(require 'helm)
(require 'helm-config)

(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 25)

(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)

(setq helm-split-window-in-side-p t)

(setq helm-boring-buffer-regexp-list
      '("\\` " "\\*scratch\\*" "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))

(global-set-key (kbd "C-S-p") 'helm-M-x)
(global-set-key (kbd "C-p") 'helm-buffers-list)

(provide 'config-helm)
