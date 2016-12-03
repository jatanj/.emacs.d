(require 'company)

(setq company-frontends
      '(company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(setq company-require-match nil)

(general-define-key
 :keymaps 'company-active-map
 "<tab>" (general-simulate-keys "<return>"))

(provide 'config-company)
