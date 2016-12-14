(require 'company)

(setq company-frontends
      '(company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(setq company-require-match nil)

(add-hook 'after-init-hook 'global-company-mode)

(general-define-key
 :keymaps 'company-active-map
 "<tab>" 'company-complete
 "<return>" (lookup-key (current-global-map) (kbd "RET")))

(provide 'config-company)
