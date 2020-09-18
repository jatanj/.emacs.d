(use-package company
  :ensure t
  :config
  (setq company-frontends '(company-pseudo-tooltip-frontend))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-require-match nil)
  (setq company-auto-complete nil)
  (setq company-tooltip-align-annotations t)
  (setq company-selection-wrap-around t)
  (setq company-eclim-auto-save nil)
  (setq company-dabbrev-downcase nil)
  (setq company-backends '(company-capf))

  (defun config/company-mode-init ()
    (setq-local completion-styles '(emacs22)))
  (add-hook 'company-mode-hook #'config/company-mode-init)

  (remove-hook 'completion-at-point-functions #'tags-completion-at-point-function)

  (defun config/company-completion-started (&rest args)
    (when (bound-and-true-p flycheck-mode)
      (setq flycheck-display-errors-function nil)))
  (defun config/company-completion-finished (&rest args)
    (when (bound-and-true-p flycheck-mode)
      (setq flycheck-display-errors-function
            #'flycheck-display-error-messages-unless-error-list)))
  (add-hook 'company-completion-started-hook #'config/company-completion-started)
  (add-hook 'company-completion-finished-hook #'config/company-completion-finished)
  (add-hook 'company-completion-cancelled-hook #'config/company-completion-finished)

  (defun company-complete-selection-or-indent ()
    (interactive)
    (if (and (bound-and-true-p company-mode)
             (or (company-tooltip-visible-p)
                 (and (bound-and-true-p company-box-mode)
                      (frame-visible-p (company-box--get-frame)))))
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
   "<return>" (general-simulate-key "RET" :state 'insert))
  (general-define-key
   :states 'insert
   "<tab>" 'company-complete-selection-or-indent))

(use-package company-box
  :ensure t
  :init
  (add-hook 'company-mode-hook #'company-box-mode)
  :config
  (setq company-box-frame-behavior 'default)
  (setq company-box-show-single-candidate 'always)
  (setq company-box-highlight-prefix t)
  (setq company-box-max-candidates 50)
  (setq company-box-backends-colors nil)
  (setq company-box-icons-alist 'company-box-icons-images)
  (setq company-box-scrollbar nil)
  (setq company-box-doc-enable nil)
  (setq company-box-tooltip-minimum-width 60)
  (setq company-box-tooltip-maximum-width 120))

(use-package company-quickhelp
  :ensure t
  :after company
  :init
  (setq company-quickhelp-delay nil))

(provide 'config-company)
