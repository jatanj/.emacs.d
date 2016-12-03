(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(idle-change mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (company-mode 1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq company-tooltip-align-annotations t)

(setq typescript-indent-level 2)
(setq tide-format-options
  '(:indentSize 2
    :tabSize 2
    :convertTabsToSpaces t))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
  (lambda ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode))))

(provide 'config-typescript)
