(defun config/docview-init ()
  (linum-mode -1))

(add-hook 'doc-view-mode-hook #'config/docview-init)

(provide 'config-docview)
