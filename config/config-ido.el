(use-package ido
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t))

(use-package flx-ido
  :config
  (flx-ido-mode 1))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

(provide 'config-ido)
