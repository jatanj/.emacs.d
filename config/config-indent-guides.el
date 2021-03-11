(use-package highlight-indent-guides
  :straight t
  :init
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-dots)
  (setq highlight-indent-guides-character ?|)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(provide 'config-indent-guides)
