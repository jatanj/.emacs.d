(use-package magit
  :ensure t
  :init
  (add-hook 'magit-mode-hook
    (lambda ()
      (tabbar-disable)
      (hscroll-mode -1)))
  (add-hook 'magit-popup-mode-hook #'tabbar-disable)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-auto-revert-mode -1) ; We already use global-auto-revert-mode
  (evil-set-initial-state 'magit-mode 'emacs)
  (general-define-key
   :keymaps 'magit-mode-map
   "q" 'magit-mode-daemon-bury-or-quit))

(defun magit-mode-daemon-bury-or-quit ()
  (interactive)
  (let ((buffers (-remove
                  (lambda (buffer) (or (eq (with-current-buffer buffer major-mode) 'dired-mode)
                                       (string-match "\\*.+" (buffer-name buffer))))
                    (buffer-list))))
    (if (and
         (daemonp)
         (= (length buffers) 0)
         (string-prefix-p "*magit:" (buffer-name (current-buffer))))
        (delete-frame)
      (magit-mode-bury-buffer))))

(use-package evil-magit
  :ensure t
  :defer t
  :after magit
  :config
  (evil-define-key evil-magit-state magit-mode-map (kbd "C-k") nil)
  (general-define-key
   :states '(magit normal visual)
   :keymaps 'magit-mode-map
   "C-<tab>" (general-simulate-keys "^ <tab>")
   "C-S-<tab>" 'magit-section-cycle-diffs
   "S-<up>" (lambda () (interactive) (evil-previous-line 10))
   "S-<down>" (lambda () (interactive) (evil-next-line 10))
   "S-<left>" (lambda () (interactive) (evil-backward-char 10))
   "S-<right>" (lambda () (interactive) (evil-forward-char 10))))

(use-package git-commit
  :ensure t
  :init
  (add-hook 'git-commit-mode-hook
    (lambda ()
      (tabbar-disable)
      (toggle-save-place 0)
      (require 'ispell)
      (when (executable-find ispell-program-name)
        (git-commit-turn-on-flyspell))
      (setq fill-column 72)
      (git-commit-turn-on-auto-fill)
      (fci-mode)))
  :config
  (global-git-commit-mode)
  (setq vc-follow-symlinks t))

(use-package gitconfig-mode
  :ensure t
  :defer t
  :mode ("\\.gitconfig\\'" . gitconfig-mode)
  :init
  (add-hook 'gitconfig-mode-hook
    (lambda ()
      (when (bound-and-true-p electric-indent-mode)
        (electric-indent-local-mode)))))

(use-package gitignore-mode
  :ensure t
  :defer t
  :mode ("\\.gitignore\\'" . gitignore-mode)
  :init
  (add-hook 'gitconfig-mode-hook
    (lambda ()
      (when (bound-and-true-p electric-indent-mode)
        (electric-indent-local-mode)))))

(provide 'config-magit)
