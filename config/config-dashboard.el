(use-package dashboard
  :ensure t
  :after evil
  :config
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (dashboard-setup-startup-hook))

(provide 'config-dashboard)
