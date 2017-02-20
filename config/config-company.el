(use-package company
  :config
  (setq company-frontends
        '(company-pseudo-tooltip-frontend))

  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)

  (setq company-idle-delay 0.1)
  (setq company-require-match nil)
  (setq company-auto-complete nil)

  (defun company-complete-selection-or-indent ()
    (interactive)
    (if (company-tooltip-visible-p)
        (company-complete-selection)
      (tab-to-tab-stop)))

  (general-define-key
   :keymaps 'company-active-map
   "<f1>" nil
   "<tab>" nil
   "<return>" (lookup-key (current-global-map) (kbd "RET"))))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-color-background "#1e1c25") ;434052
  (setq company-quickhelp-color-foreground "#f8f8f8"))

(provide 'config-company)
