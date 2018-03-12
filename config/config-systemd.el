(use-package systemd
  :ensure t
  :mode (("\\.service\\'" . systemd-mode))
  :init
  (add-hook 'systemd-mode-hook
            (lambda ()
              (company-mode 1)
              (flycheck-mode 1))))

(provide 'config-systemd)
