(use-package fill-column-indicator
  :straight t
  :config
  (setq-default fill-column 80)
  (setq fci-rule-color "#363342")
  (setq fci-rule-use-dashes t))

(provide 'config-fill-column-indicator)
