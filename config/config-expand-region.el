(use-package expand-region
  :ensure t
  :config
  (general-define-key
   :states '(normal visual)
   "=" 'er/expand-region
   "-" 'er/contract-region))

(provide 'config-expand-region)
