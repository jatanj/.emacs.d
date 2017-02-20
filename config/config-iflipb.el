(use-package iflipb
  :config
  (setq iflipb-wrap-around t)
  (global-set-key (kbd "C-<tab>") 'iflipb-next-buffer)
  (global-set-key (kbd "<C-iso-lefttab>") 'iflipb-previous-buffer))

(provide 'config-iflipb)
