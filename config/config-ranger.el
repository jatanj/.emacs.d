(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-eagerly t)
  (add-hook 'ranger-mode-hook (lambda () (tabbar-blend-header-line "Dired")))

  (put 'dired-find-alternate-file 'disabled nil)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  (general-define-key
   :keymaps 'ranger-mode-map
   "C-k" ctl-x-map))

(provide 'config-ranger)
