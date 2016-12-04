(add-to-list 'display-buffer-alist
             `("\\*Flycheck errors\\*"
               (display-buffer-reuse-window
                display-buffer-below-selected)
               (window-height   . 0.15)))

(provide 'config-flycheck)
