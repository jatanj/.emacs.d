(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?|)
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

(provide 'config-indent-guides)
