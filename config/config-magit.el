(use-package magit
  :ensure t
  :init
  (add-hook 'magit-mode-hook
    (lambda ()
      (tabbar-blend-header-line "Magit")
      (hscroll-mode -1)))
  (add-hook 'magit-popup-mode-hook #'tabbar-local-disable)
  (add-hook 'magit-blob-mode-hook #'tabbar-blend-header-line)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (dolist (action '((?d "Diff..." magit-diff-buffer-file-popup)
                    (?D "Diff"    magit-diff-buffer-file)
                    (?g "Gutter"  toggle-global-git-gutter-mode)))
    (apply 'magit-define-popup-action 'magit-file-popup action))
  (magit-auto-revert-mode -1) ; We already use global-auto-revert-mode
  (evil-set-initial-state 'magit-mode 'emacs)
  (defhydra hydra-magit-blob (magit-blob-mode-map "g t")
    ("p" magit-blob-previous "Previous")
    ("n" magit-blob-next "Next")
    ("q" magit-kill-all-blobs "Quit"))
  (general-define-key
   :keymaps 'magit-mode-map
   "q" 'magit-daemon-bury-or-quit))

(defun magit-kill-all-blobs ()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p magit-blob-mode)
        (magit-kill-this-buffer)))))

(defun magit-daemon-bury-or-quit ()
  (interactive)
  (let ((buffers (-remove
                  (lambda (buffer) (or (eq (with-current-buffer buffer major-mode) 'dired-mode)
                                       (string-match "\\*.+" (buffer-name buffer))))
                  (buffer-list))))
    (if (and (daemonp)
             (= (length buffers) 0)
             (string-prefix-p "*magit:" (buffer-name)))
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
      (tabbar-blend-header-line "Commit")
      (toggle-save-place 0)
      (require 'ispell)
      (when (executable-find ispell-program-name)
        (git-commit-turn-on-flyspell))
      (turn-on-proselint)
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
  (add-hook 'gitignore-mode-hook
    (lambda ()
      (setq indent-line-function 'indent-relative-maybe))))

(use-package git-gutter-fringe
  :ensure t)

(defun toggle-global-git-gutter-mode ()
  (interactive)
  (if (bound-and-true-p git-gutter-mode)
      (global-git-gutter-mode -1)
    (global-git-gutter-mode)))

(provide 'config-magit)
