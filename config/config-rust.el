(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook
    (lambda ()
      (set-local-tab-width 4))))

(provide 'config-rust)
