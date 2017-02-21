(use-package js2-mode
  :ensure t
  :defer t
  :after tide
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js-indent-level 2)
  (setq javascript-indent-level 2)
  :config
  (setq js2-basic-offset 2)
  (setq js2-mode-assume-strict t)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-idle-timer-delay 0.2)
  (add-hook 'js2-mode-hook #'setup-tide-mode))

(provide 'config-javascript)
