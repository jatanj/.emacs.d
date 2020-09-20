(use-package ido
  :straight t
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t))

(use-package flx-ido
  :straight t
  :after ido
  :config
  (flx-ido-mode 1))

(use-package ido-vertical-mode
  :straight t
  :after ido
  :config
  (ido-vertical-mode 1))

(provide 'config-ido)
