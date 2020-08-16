(use-package helpful
  :ensure t
  :init
  (require 'help)
  (setq help-window-select t)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Help" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (side . bottom)
                 (window-height . 0.35)))
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

(provide 'config-help)
