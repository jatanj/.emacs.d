(use-package lsp-mode
  :ensure t)

(use-package lsp-intellij
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'java-mode-hook #'lsp-intellij-enable))

(use-package company-lsp
  :ensure t
  :init
  (add-hook 'lsp-mode-hook #'company-mode)
  :config
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-cache-candidates t)
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook
            (lambda ()
              (lsp-ui-mode 1)
              (flycheck-mode 1))))

(provide 'config-lsp)
