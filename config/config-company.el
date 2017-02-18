(require 'company)
(require 'company-quickhelp)

(setq company-frontends
      '(company-pseudo-tooltip-frontend))

(add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)

(setq company-idle-delay 0.1)
(setq company-require-match nil)
(setq company-auto-complete nil)

(company-quickhelp-mode 1)
(setq company-quickhelp-color-background "#434052")
(setq company-quickhelp-color-foreground "#f8f8f8")

(general-define-key
 :keymaps 'company-active-map
 "<f1>" nil
 "<return>" (lookup-key (current-global-map) (kbd "RET")))

(provide 'config-company)
