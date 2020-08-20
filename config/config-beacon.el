(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-blink-duration 0.3)
  (setq beacon-blink-delay 0)
  (setq beacon-blink-when-window-scrolls nil)
  (setq beacon-blink-when-buffer-changes nil)
  (add-hook 'config/configure-frame-functions
    (lambda (frame)
      (setq beacon-blink-when-point-moves-vertically
            (/ (display-pixel-height) (line-pixel-height) 2)))))

(provide 'config-beacon)
