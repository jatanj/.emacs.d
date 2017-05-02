(use-package cc-mode
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (customize-cc-mode)))
  :config
  (general-define-key
   :keymaps 'java-mode-map
   :states 'insert
   "C-<return>" 'c-indent-new-comment-line))

(use-package meghanada
  :ensure t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (meghanada-mode t)
              (flycheck-mode 1)
              (setq-local local-jump-to-definition 'meghanada-jump-declaration))))

(use-package groovy-mode
  :ensure t
  :mode (("\\.gradle\\'" . groovy-mode)
         ("\\.groovy\\'" . groovy-mode))
  :init
  (add-hook 'groovy-mode-hook
            (lambda ()
              (set-local-tab-width 4))))

(provide 'config-java)
