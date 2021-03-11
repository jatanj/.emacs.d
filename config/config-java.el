(use-package cc-mode
  :init
  (defun config/java-mode-init ()
    (config/set-local-tab-width 4))
  (add-hook 'java-mode-hook #'config/java-mode-init)
  :config
  (general-define-key
   :keymaps 'java
   :states 'insert
   "C-<return>" 'c-indent-new-comment-line))

(use-package groovy-mode
  :straight t
  :mode (("\\.gradle\\'" . groovy-mode)
         ("\\.groovy\\'" . groovy-mode))
  :init
  (add-hook 'groovy-mode-hook
            (lambda ()
              (config/set-local-tab-width 4))))

(provide 'config-java)
