(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 35)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-split-window-in-side-p t)
  (setq helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (side . bottom)
                 (window-height . 0.35)))
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
  (setq helm-ag-base-command "rg --smart-case --no-heading")
  (general-define-key
   :keymaps 'projectile-command-map
   "s s" 'helm-do-ag-project-root
   "s f" 'helm-do-ag-this-file
   "s b" 'helm-do-ag-buffers))

(use-package helm-swoop
  :ensure t
  :after helm
  :config
  (setq helm-swoop-split-window-function
        (lambda ($buf)
          (display-buffer $buf)))
  (add-to-list 'display-buffer-alist
               `("\\`\\*Helm Swoop\\*\\'"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (side . bottom)
                 (window-height . 0.35)))
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-command-map (make-sparse-keymap))
  (general-define-key
   :keymaps 'helm-swoop-map
   "C-e" 'helm-swoop-edit)
  (general-define-key
   :keymaps 'helm-swoop-command-map
   "p" 'helm-multi-swoop-projectile
   "f" 'helm-swoop
   "b" 'helm-multi-swoop-all))

(provide 'config-helm)
