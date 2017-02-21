(use-package uniquify
  :init
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; (setq uniquify-buffer-name-style 'forward)
;; (setq uniquify-separator "/")

(provide 'config-uniquify)
