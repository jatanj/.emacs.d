(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-eagerly t)
  (general-define-key
   :keymaps 'ranger-mode-map
   "C-k" ctl-x-map))

(provide 'config-ranger)
