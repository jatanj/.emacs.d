(use-package neotree
  :ensure t
  :after doom-themes
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'ascii))
  (setq neo-window-fixed-size nil)
  (setq neo-show-hidden-files t)
  (setq doom-neotree-enable-variable-pitch nil)
  (setq doom-neotree-file-icons 'simple)
  (setq doom-neotree-line-spacing 0)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (setq mode-line-format nil)
      (linum-mode -1)
      (display-line-numbers-mode -1)
      (hscroll-mode 1)
      (setq buffer-face-mode-face `(:background ,(car (get 'custom-theme-color-bg6 'saved-value)) :family "Fira Code Medium" :height 100))
      (buffer-face-mode 1)))
  :config
  (dolist (func '(switch-to-buffer
                  previous-buffer
                  kill-this-buffer))
    (advice-add func :after (lambda (&rest _)
                              (neotree-switch-to-project-root))))
  (general-define-key
   :keymaps 'neotree-mode-map
   "C-f" 'isearch-forward-regexp
   "C-l C-a" 'delete-window
   "C-<prior>" 'ignore
   "C-<next>" 'ignore))

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
        (when (and dir-name (file-directory-p dir-name))
          (if (neo-global--window-exists-p)
              (with-selected-window neo-global--window
                (let ((read-only buffer-read-only))
                  (setq buffer-read-only nil)
                  (save-selected-window
                    (neo-buffer--change-root dir-name))
                  (setq buffer-read-only read-only)))
            (neotree-dir dir-name))))))

(defun neotree-projectile (&optional switch-only)
  "Open neotree with projectile as root and open node for current file.
  If projectile unavailable or not in a project, open node at file path.
  If file path is not available, open $HOME."
  (interactive)
  (if (and (not switch-only) (neo-global--window-exists-p))
      (call-interactively 'neotree-hide)
    (neotree-switch-to-project-root t)))

(defun neotree-projectile-find ()
  (interactive)
  (let* ((file (buffer-file-name (current-buffer))))
    (neotree-projectile t)
    (neotree-find file)))

(provide 'config-neotree)
