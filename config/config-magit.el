(use-package magit-popup
  :straight t)

(use-package magit
  :straight t
  :after magit-popup
  :init
  (add-hook 'magit-status-mode-hook
    (lambda ()
      (tabbar-blend-header-line "Magit")
      (hscroll-mode -1))
    t)

  (dolist (hook '(magit-popup-mode-hook
                  magit-process-mode-hook
                  magit-status-mode-hook
                  magit-diff-mode-hook
                  magit-log-mode-hook
                  magit-blob-mode-hook
                  magit-blame-mode-hok
                  magit-revision-mode-hook))
    (add-hook hook #'centaur-tabs-local-disable))
  (add-hook 'magit-file-mode-hook
            (lambda ()
              (if (not buffer-file-name)
                  (centaur-tabs-local-mode))))

  (advice-add 'magit-set-header-line-format :around
              (lambda (orig &rest args)
                (apply 'centaur-tabs-blend-header-line args)))

  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (dolist (action '((?d "Diff..." magit-diff-buffer-file-popup)
                    (?D "Diff"    magit-diff-buffer-file)
                    (?g "Gutter"  toggle-global-git-gutter-mode)))
    (apply 'magit-define-popup-action 'magit-file-popup action))

  (magit-auto-revert-mode -1) ; We already use global-auto-revert-mode
  (evil-set-initial-state 'magit-mode 'emacs)

  (setq magit-rebase-arguments '("--committer-date-is-author-date"))

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
  :straight t
  :after magit
  :config
  (evil-define-key evil-magit-state magit-mode-map (kbd "C-k") nil)
  (general-define-key
   :states '(magit normal visual)
   :keymaps 'magit-mode-map
   "C-<tab>" (general-simulate-key "^ <tab>")
   "C-S-<tab>" 'magit-section-cycle-diffs
   "S-<up>" (lambda () (interactive) (evil-previous-line 10))
   "S-<down>" (lambda () (interactive) (evil-next-line 10))
   "S-<left>" (lambda () (interactive) (evil-backward-char 10))
   "S-<right>" (lambda () (interactive) (evil-forward-char 10))))

(use-package git-commit
  :straight t
  :init
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (centaur-tabs-local-mode)
              (centaur-tabs-blend-header-line "Magit - Commit")
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

; (use-package gitconfig-mode
;   :straight t
;   :defer t
;   :mode ("\\.gitconfig\\'" . gitconfig-mode)
;   :init
;   (add-hook 'gitconfig-mode-hook
;     (lambda ()
;       (when (bound-and-true-p electric-indent-mode)
;         (electric-indent-local-mode)))))

; (use-package gitignore-mode
;   :straight t
;   :defer t
;   :mode ("\\.gitignore\\'" . gitignore-mode)
;   :init
;   (add-hook 'gitignore-mode-hook
;     (lambda ()
;       (setq indent-line-function 'indent-relative-maybe))))

; (use-package git-gutter-fringe
;   :straight t
;   :init
;   (setq git-gutter-fr:side 'right-fringe)
;   ;; https://github.com/hlissner/doom-emacs
;   (defun git-gutter-maybe ()
;     (when (and (buffer-file-name)
;                (not (file-remote-p (buffer-file-name))))
;       (setq-default fringes-outside-margins t)
;       (set-window-buffer (get-buffer-window) (current-buffer))
;       (git-gutter-mode +1)))
;   (-map (lambda (x) (add-hook x #'git-gutter-maybe))
;         '(text-mode-hook prog-mode-hook conf-mode-hook))
;   (add-hook 'focus-in-hook #'git-gutter:update-all-windows)
;   :config
;   (fringe-helper-define 'git-gutter-fr:added '(center repeated)
;     "XXX.....")
;   (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
;     "XXX.....")
;   (fringe-helper-define 'git-gutter-fr:deleted 'bottom
;     "X......."
;     "XX......"
;     "XXX....."
;     "XXXX...."))

(use-package fringe-helper
  :straight t)

(defun git-gutter-global-toggle ()
  (interactive)
  (if (bound-and-true-p git-gutter-mode)
      (global-git-gutter-mode -1)
    (global-git-gutter-mode)))

(provide 'config-magit)
