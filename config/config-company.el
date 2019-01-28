(use-package company
  :ensure t
  :config
  (setq company-frontends
        '(company-pseudo-tooltip-frontend))
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.35)
  (setq company-minimum-prefix-length 3)
  (setq company-require-match nil)
  (setq company-auto-complete nil)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t)
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  (general-define-key "C-j" nil)
  (general-define-key
   :keymaps 'company-mode-map
   "C-SPC" 'company-complete)
  (general-define-key
   :keymaps 'company-active-map
   "<escape>" 'company-quit
   "<tab>" nil
   "<return>" (lookup-key (current-global-map) (kbd "RET"))))

(defun company-quit ()
  (interactive)
  (company-cancel)
  (evil-normal-state))

(defun company-complete-selection-or-indent ()
  (interactive)
  (if (company-tooltip-visible-p)
      (company-complete-selection)
    (tab-to-tab-stop)))

(use-package company-statistics
  :ensure t
  :init
  (company-statistics-mode 1))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1))

(provide 'config-company)
