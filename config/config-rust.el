(use-package rust-mode
  :straight t
  :init
  (add-hook 'rust-mode-hook
    (lambda ()
      (config/set-local-tab-width 4))))

(use-package racer
  :straight t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook
            (lambda ()
              (company-mode 1)
              (eldoc-mode 1)
              (setq-local company-quickhelp-delay nil)))
  :config
  (add-to-list 'config/quick-kill-buffer-list
               '("\\`\\*Racer Help\\*\\'" racer-help-mode))
  (general-define-key
   :keymaps 'racer-mode-map
   "C-c C-d" 'racer-describe))

(use-package flycheck-rust
  :straight t
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (flycheck-mode 1)
              (flycheck-rust-setup))))

(provide 'config-rust)
