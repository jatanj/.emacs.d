(use-package treemacs
  :straight t
  :init
  (setq treemacs-show-cursor t)
  (setq treemacs-show-hidden-files t)
  (setq treemacs-space-between-root-nodes nil)

  (add-hook 'config/configure-frame-functions
            (lambda (&rest _) (setq treemacs-width (if (> (x-display-pixel-width) 1920) 60 35))))

  (defun config/treemacs-is-treemacs-buffer? (&optional buffer)
    (let ((buffer (or buffer (current-buffer))))
      (s-starts-with? "*Treemacs" (s-trim-left (buffer-name buffer)))))

  (defun config/treemacs-expand-all-projects (&optional arg)
    (interactive)
    (save-excursion
      (treemacs--forget-last-highlight)
      (let ((workspace (treemacs-current-workspace)))
        (when (hash-table-p workspace)
          (dolist (project (treemacs-workspace->projects workspace))
            (-when-let (pos (treemacs-project->position project))
              (when (eq 'root-node-closed (treemacs-button-get pos :state))
                (goto-char pos)
                (treemacs--expand-root-node pos))))))
      (treemacs--maybe-recenter 'on-distance)))

  (defun config/treemacs-toggle ()
    (interactive)
    (pcase (treemacs-current-visibility)
      ('visible (if (config/treemacs-is-treemacs-buffer?)
                    (delete-window (get-buffer-window (current-buffer)))
                  (treemacs-select-window)))
      ('exists  (treemacs-select-window))
      ('none    (treemacs--init))))

  (defun config/treemacs-toggle-find-file (&optional ARG)
    (interactive)
    (unless (s-starts-with? "*Treemacs" (s-trim-left (buffer-name)))
      (treemacs-find-file))
    (config/treemacs-toggle))

  (defun config/treemacs-toggle-find-file-collapse-other-projects ()
    (interactive)
    (let ((buffer (current-buffer)))
      (when (not (config/treemacs-is-treemacs-buffer? buffer))
        (config/treemacs-toggle))
      (config/treemacs-expand-all-projects)
      (switch-to-buffer buffer))
    (config/treemacs-toggle-find-file)
    (treemacs-collapse-other-projects)
    (treemacs--maybe-recenter 'always))

  (defun config/treemacs-init ()
    (set-window-fringes (get-buffer-window (current-buffer)) 0 0 nil)
    (setq-local left-fringe-width 0)
    (setq-local right-fringe-width 0)
    (linum-mode -1)
    (hl-line-mode -1)
    (display-line-numbers-mode -1)
    (hscroll-mode 1)
    (setq buffer-face-mode-face `(:background ,(car (get 'custom-theme-color-bg6 'saved-value))
                                  :family ,(car (s-split "-" local-menu-font-face))
                                  :height 100))
    (buffer-face-mode 1))

  (add-hook 'treemacs-mode-hook #'config/treemacs-init)

  :config
  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode -1)
  (treemacs-git-mode 'deferred)

  ;; Disable delete keybinding in treemacs-mode.
  (general-define-key
   :keymaps 'treemacs-mode-map
   "d" nil
   "D" nil))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile)
  :init)

(use-package treemacs-evil
  :straight t
  :after (treemacs evil)
  :init)

(use-package treemacs-magit
  :straight t
  :after (treemacs magit)
  :init)

(use-package treemacs-all-the-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(provide 'config-treemacs)
