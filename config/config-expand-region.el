(use-package expand-region
  :config
  (general-define-key
   :states '(normal visual)
   "=" 'er/expand-region
   "-" 'er/contract-region))

(provide 'config-expand-region)
