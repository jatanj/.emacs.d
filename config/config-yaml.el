(use-package yaml-mode
  :straight t
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  :config
  (general-define-key
   :keymaps 'yaml-mode-map))

(provide 'config-yaml)
