(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'ascii))
  (setq neo-window-fixed-size nil)
  :config
  (general-define-key
    :keymaps 'neotree-mode-map
    "C-<prior>" 'ignore
    "C-<next>" 'ignore))

(defun neotree-projectile ()
  "Open neotree with projectile as root and open node for current file.
  If projectile unavailable or not in a project, open node at file path.
  If file path is not available, open $HOME."
  (interactive)
  (if (neo-global--window-exists-p)
      (call-interactively 'neotree-hide)
    (let ((file-name (buffer-file-name)))
      (if (and (not file-name)
               (let ((buffer-name (buffer-name)))
                 (cond
                  ((equal buffer-name "*cider-repl server*") nil)
                  (t t))))
          (neotree-dir local-directory)
        (let ((dir-name (if (and (fboundp 'projectile-project-p)
                                 (projectile-project-p))
                            (projectile-project-root)
                          (file-name-directory file-name))))
          (neotree-dir dir-name)
          (when file-name
            (neo-buffer--select-file-node file-name)))))))


(provide 'config-neotree)
