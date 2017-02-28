(dolist (assoc '(("PKGBUILD" . shell-script-mode)))
  (add-to-list 'auto-mode-alist assoc))

(add-hook 'sh-mode-hook #'flycheck-mode)

(provide 'config-shell)
