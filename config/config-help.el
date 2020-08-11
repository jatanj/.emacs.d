(use-package helpful
  :ensure t
  :init
  (require 'help)
  (setq help-window-select t)
  (add-to-list 'configure-display-buffer-alist
               '("\\`\\*Help\\*\\'" help-mode))
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

(provide 'config-help)
