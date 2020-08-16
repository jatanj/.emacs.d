(use-package company
  :ensure t
  :config
  (setq company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 0)
  (setq company-require-match nil)
  (setq company-auto-complete nil)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t)
  (setq company-eclim-auto-save nil)
  (setq company-dabbrev-downcase nil)
  (setq company-backends '(company-capf))
  (remove-hook 'completion-at-point-functions #'tags-completion-at-point-function)

  (defun company-complete-selection-or-indent ()
    (interactive)
    (if (company-tooltip-visible-p)
        (company-complete-selection)
      (tab-to-tab-stop)))

  (defun company-quit ()
    (interactive)
    (company-cancel)
    (evil-normal-state))

  (general-define-key "C-j" nil)
  (general-define-key
   :keymaps 'company-mode-map
   "C-SPC" 'company-complete)
  (general-define-key
   :keymaps 'company-active-map
   "<escape>" 'company-quit
   "<tab>" nil
   "<return>" (lookup-key (current-global-map) (kbd "RET"))))

(use-package company-quickhelp
  :ensure t
  :after company
  :init
  (setq company-quickhelp-delay nil))

(provide 'config-company)
