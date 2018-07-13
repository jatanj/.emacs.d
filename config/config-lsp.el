(use-package lsp-mode
  :ensure t)

(use-package lsp-intellij
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'java-mode-hook #'lsp-intellij-enable))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-hover nil)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (lsp-ui-mode 1)
              (lsp-ui-sideline-mode 1)
              (company-mode 1)
              (flycheck-mode 1))))

(provide 'config-lsp)
