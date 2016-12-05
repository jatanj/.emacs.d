(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.125)))

(evil-set-initial-state 'flycheck-error-list-mode 'emacs)

(provide 'config-flycheck)
