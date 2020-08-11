(use-package treemacs
  :ensure t
  :init
  (setq treemacs-show-cursor t)
  (setq treemacs-show-hidden-files t)
  (setq treemacs-space-between-root-nodes nil)

  (defun treemacs-expand-all-projects (&optional arg)
    (interactive)
    (save-excursion
      (treemacs--forget-last-highlight)
      (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
        (-when-let (pos (treemacs-project->position project))
          (when (eq 'root-node-closed (treemacs-button-get pos :state))
            (goto-char pos)
            (treemacs--expand-root-node pos)))))
    (treemacs--maybe-recenter 'on-distance))

  (defun treemacs-toggle ()
    (interactive)
    (pcase (treemacs-current-visibility)
      ('visible (if (s-starts-with? "*Treemacs" (s-trim-left (buffer-name)))
                    (delete-window (treemacs-get-local-window))
                  (treemacs-select-window)))
      ('exists  (treemacs-select-window))
      ('none    (treemacs--init))))

  (defun treemacs-toggle-find-file (&optional ARG)
    (interactive)
    (unless (s-starts-with? "*Treemacs" (s-trim-left (buffer-name)))
      (treemacs-find-file))
    (treemacs-toggle))

  (defun treemacs-toggle-find-file-collapse-other-projects ()
    (interactive)
    (let ((buffer (current-buffer)))
      (treemacs-toggle)
      (treemacs-expand-all-projects)
      (switch-to-buffer buffer))
    (treemacs-toggle-find-file)
    (treemacs-collapse-other-projects)
    (treemacs--maybe-recenter 'always))

  (add-hook 'treemacs-mode-hook
    (lambda ()
      (set-window-fringes (get-buffer-window (current-buffer)) 0 0 nil)
      (setq-local left-fringe-width 0)
      (setq-local right-fringe-width 0)
      (linum-mode -1)
      (hl-line-mode -1)
      (display-line-numbers-mode -1)
      (hscroll-mode 1)
      (setq buffer-face-mode-face `(:background ,(car (get 'custom-theme-face-bg6 'saved-value))
                                    :family "Fira Code Medium"
                                    :height 100))
      (buffer-face-mode 1)))

  ;; (dolist (func '(switch-to-buffer
  ;;                 previous-buffer
  ;;                 kill-this-buffer))
  ;;   (advice-add func :after
  ;;               (lambda (&rest _)
  ;;                 (if (projectile-project-p)
  ;;                     (treemacs-display-current-project-exclusively)))))

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
  :ensure t
  :after treemacs projectile
  :init)

(use-package treemacs-evil
  :ensure t
  :after treemacs evil
  :init)

(use-package treemacs-magit
  :ensure t
  :after treemacs magit
  :init)

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(provide 'config-treemacs)
