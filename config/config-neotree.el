(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'ascii))
  (setq neo-window-fixed-size nil)
  (setq neo-show-hidden-files t)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (linum-mode -1)
      (tabbar-blend-header-line "Folders")))
  :config
  (general-define-key
   :keymaps 'neotree-mode-map
   "C-f" 'isearch-forward-regexp
   "C-<prior>" 'ignore
   "C-<next>" 'ignore))

(dolist (func '(switch-to-buffer
                previous-buffer
                kill-this-buffer))
  (advice-add func :after (lambda (&rest _)
                            (neotree-switch-to-project-root))))

(defun neotree-switch-to-project-root (&optional show)
  "Switch the neotree buffer root directory to the projectile project root.
When SHOW is t, the neotree buffer will be shown if it's currently hidden."
  (interactive)
  (when (or (neo-global--window-exists-p) show)
      (let* ((file-name (buffer-file-name))
             (dir-name (cond
                        ((active-minibuffer-window) nil)
                        ((numberp (string-match-p "\\*.*\\*" (buffer-name))) nil)
                        ((not file-name) (expand-file-name local-directory))
                        (t (if (and (fboundp 'projectile-project-p)
                                    (projectile-project-p))
                               (projectile-project-root)
                             (file-name-directory file-name))))))
        (when dir-name
          (if (neo-global--window-exists-p)
              (with-selected-window neo-global--window
                (let ((read-only buffer-read-only))
                  (setq buffer-read-only nil)
                  (save-selected-window
                    (neo-buffer--change-root dir-name))
                  (setq buffer-read-only read-only)))
            (neotree-dir dir-name))))))

(defun neotree-projectile ()
  "Open neotree with projectile as root and open node for current file.
  If projectile unavailable or not in a project, open node at file path.
  If file path is not available, open $HOME."
  (interactive)
  (if (neo-global--window-exists-p)
      (call-interactively 'neotree-hide)
    (neotree-switch-to-project-root t)))

(provide 'config-neotree)
