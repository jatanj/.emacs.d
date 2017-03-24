(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-blink-duration 0.3)
  (setq beacon-blink-delay 0)
  (setq beacon-blink-when-window-scrolls nil)
  (setq beacon-blink-when-buffer-changes nil)
  (defun beacon-set-blink-vertically (&optional global)
    (if global
        (setq-default beacon-blink-when-point-moves-vertically
                      (/ (display-pixel-height) (line-pixel-height)))
      (setq-local beacon-blink-when-point-moves-vertically
                  (/ (window-body-height) 2))))
  (add-hook 'after-make-frame-functions
    (lambda () (beacon-set-blink-vertically t)))
  (add-hook 'post-command-hook
    (lambda ()
      (when (bound-and-true-p beacon-mode)
        (when (and beacon--previous-place
                    (not (eq (marker-buffer beacon--previous-place)
                            (current-buffer))))
          (beacon-set-blink-vertically))))))

(provide 'config-beacon)
