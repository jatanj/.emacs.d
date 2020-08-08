(use-package cc-mode
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (customize-cc-mode)
              (rainbow-delimiters-mode-enable)))
  :config
  (general-define-key
   :keymaps 'java-mode-map
   :states 'insert
   "C-<return>" 'c-indent-new-comment-line))

(use-package groovy-mode
  :ensure t
  :mode (("\\.gradle\\'" . groovy-mode)
         ("\\.groovy\\'" . groovy-mode))
  :init
  (add-hook 'groovy-mode-hook
            (lambda ()
              (set-local-tab-width 4))))

(provide 'config-java)
