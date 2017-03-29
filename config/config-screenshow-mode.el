(use-package screenshow-mode
  :config
  (add-to-list 'screenshow-ignore-buffer-list-regexp "\\*NeoTree\\*")
  (add-hook 'screenshow-frame-hook
            (lambda (frame) (set-frame-font "Inconsolata-15" t (list frame)))))

(provide 'config-screenshow-mode)
