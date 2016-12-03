(require 'iflipb)

(global-set-key (kbd "C-<tab>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-iso-lefttab>") 'iflipb-previous-buffer)

(setq iflipb-wrap-around t)

(provide 'config-iflipb)
