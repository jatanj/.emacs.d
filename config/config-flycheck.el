(add-to-list 'display-buffer-alist
             `("\\*Flycheck errors\\*"
               (display-buffer-reuse-window
                display-buffer-below-selected)
               (window-height   . 0.20)))

(evil-set-initial-state 'flycheck-error-list-mode 'emacs)

(provide 'config-flycheck)
