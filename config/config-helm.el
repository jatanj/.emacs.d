(use-package helm
  :ensure t
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
  (defun filter-buffers (buffer-list)
    (delq nil (mapcar
               (lambda (buffer)
                 (cond
                  ((eq (with-current-buffer buffer major-mode) 'dired-mode) nil)
                  (t buffer)))
               buffer-list)))
  (advice-add 'helm-skip-boring-buffers :filter-return 'filter-buffers)
  (global-set-key (kbd "C-S-p") 'helm-M-x)
  (global-set-key (kbd "C-p") 'helm-buffers-list)
  (general-define-key
   :prefix leader-key
   "f" 'helm-find))

(use-package helm-projectile
  :ensure t
  :after projectile
  :config
  (helm-projectile-on))

(use-package helm-ag
  :ensure t
  :config
  (setq helm-ag-fuzzy-match t)
  (general-define-key
   :keymaps 'projectile-command-map
   "s f" 'helm-do-ag-this-file
   "s b" 'helm-do-ag-buffers))

(provide 'config-helm)
