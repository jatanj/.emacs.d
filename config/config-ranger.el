(use-package ranger
  :straight t
  :init
  (setq ranger-show-hidden t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-modify-header nil)

  (defun config/dired-header-line ()
    (tabbar-blend-header-line
     (format "%s@%s: %s"
             user-login-name
             system-name
             (file-truename default-directory))))

  :config
  (ranger-override-dired-mode t)

  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-after-readin-hook #'config/dired-header-line)
  (evil-set-initial-state 'dired-mode 'emacs)

  (general-define-key
   :keymaps 'ranger-mode-map
   "C-k" ctl-x-map))

(use-package all-the-icons-dired
  :straight t
  :config
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(provide 'config-ranger)
