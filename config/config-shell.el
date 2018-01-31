(dolist (assoc '(("PKGBUILD" . shell-script-mode)))
  (add-to-list 'auto-mode-alist assoc))

(add-hook 'sh-mode-hook
          (lambda ()
            (flycheck-mode)
            (set-local-tab-width 4)))

(provide 'config-shell)
