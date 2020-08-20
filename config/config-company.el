(use-package company
  :ensure t
  :config
  (setq company-frontends '(company-pseudo-tooltip-frontend))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 0)
  (setq company-require-match nil)
  (setq company-auto-complete nil)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t)
  (setq company-eclim-auto-save nil)
  (setq company-dabbrev-downcase nil)
  (setq company-backends '(company-capf))
  (add-hook 'company-mode-hook
            (lambda ()
              (setq-local completion-styles '(emacs22))))
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
   "<return>" (general-simulate-key "RET" :state 'insert)))

(use-package company-box
  :ensure t
  :config
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)
  (setq company-box-backends-colors nil)
  (setq company-box-icons-alist 'company-box-icons-images)
  (setq company-box-doc-enable nil))

(use-package company-quickhelp
  :ensure t
  :after company
  :init
  (setq company-quickhelp-delay nil))

(provide 'config-company)
