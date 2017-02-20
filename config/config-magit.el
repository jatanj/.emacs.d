(use-package magit
  :after evil
  :config
  (magit-auto-revert-mode -1) ; We already use global-auto-revert-mode
  (evil-set-initial-state 'magit-mode 'emacs)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (global-git-commit-mode)
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))
  (setq vc-follow-symlinks t)

  ;; Disable tabbar-mode in magit buffers
  (add-hook 'magit-mode-hook #'tabbar-disable)
  (add-hook 'magit-popup-mode-hook #'tabbar-disable)

  (general-define-key
   :keymaps 'magit-mode-map
   "q" 'magit-mode-daemon-bury-or-quit)

  (general-define-key
   :states '(magit normal visual)
   :keymaps 'magit-mode-map
   "C-k" nil
   "C-<tab>" (general-simulate-keys "^ <tab>")
   "C-S-<tab>" 'magit-section-cycle-diffs
   "S-<up>" (lambda () (interactive) (evil-previous-line 10))
   "S-<down>" (lambda () (interactive) (evil-next-line 10))
   "S-<left>" (lambda () (interactive) (evil-backward-char 10))
   "S-<right>" (lambda () (interactive) (evil-forward-char 10))))

(use-package evil-magit
  :after magit)

(use-package gitconfig-mode)
(use-package gitignore-mode)

(defun magit-mode-daemon-bury-or-quit ()
  (interactive)
  (let ((buffers (-remove
                  (lambda (b) (string-match "\\*.+" (buffer-name b)))
                  (buffer-list))))
    (if (and
         (daemonp)
         (= (length buffers) 0)
         (string-prefix-p "*magit:" (buffer-name (current-buffer))))
        (delete-frame)
      (magit-mode-bury-buffer))))

(provide 'config-magit)
