(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook
    (lambda ()
      (set-local-tab-width 4))))

(use-package racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook
            (lambda ()
              (company-mode 1)
              (eldoc-mode 1)
              (setq-local company-quickhelp-delay nil)
              (setq-local local-jump-to-definition 'racer-find-definition)))
  :config
  (add-to-list 'configure-display-buffer-alist
               '("\\`\\*Racer Help\\*\\'" racer-help-mode))
  (general-define-key
   :keymaps 'racer-mode-map
   "C-c C-d" 'racer-describe))

(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'rust-mode-hook
            (lambda ()
              (flycheck-mode 1)
              (flycheck-rust-setup))))

(provide 'config-rust)
