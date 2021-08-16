(use-package company
  :straight t
  :config
  (setq company-frontends '(company-pseudo-tooltip-frontend))
  (setq company-tooltip-limit 15)
  (setq company-idle-delay 0.3)
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

  (defun config/company-completion-started (&rest _)
    (let ((surrounding (list (char-before) (char-after))))
      ;; Cancel company if we're inside braces, parentheses, or brackets
      (when (or (equal surrounding '(?( ?)))
                (equal surrounding '(?{ ?}))
                (equal surrounding '(?[ ?])))
        (company-cancel)))
    (when (bound-and-true-p flycheck-mode)
      (setq flycheck-display-errors-function nil)))

  (defun config/company-completion-finished (&rest _)
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
                      (when-let ((frame (company-box--get-frame)))
                        (frame-visible-p frame)))))
        (company-complete-selection)
    (tab-to-tab-stop)))

  (defun config/company-quit ()
    (interactive)
    (company-cancel)
    (evil-normal-state))

  (defun config/company-box-too-small-fix ()
    (interactive)
    (company-box-hide)
    (company-box-show))

  (general-define-key "C-j" nil)
  (general-define-key
   :keymaps 'company-mode-map
   "C-SPC" 'company-complete)
  (general-define-key
   :keymaps 'company-active-map
   "<escape>" 'config/company-quit
   "C-d" 'config/company-box-too-small-fix
   "<tab>" nil
   "<return>" (general-simulate-key "RET" :state 'insert))
  (general-define-key
   :states 'insert
   "<tab>" 'company-complete-selection-or-indent))

(use-package company-box
  :straight t
  :init
  (defun config/company-box-init (&rest _)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p company-mode)
          (company-box-mode 1))))
    (add-hook 'company-mode-hook #'company-box-mode))
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'config/company-box-init)
    (config/company-box-init))
  :config
  (setq company-box-frame-behavior 'default)
  (setq company-box-show-single-candidate 'always)
  (setq company-box-highlight-prefix t)
  (setq company-box-max-candidates 30)
  (setq company-box-backends-colors nil)
  (setq company-box-icons-alist 'company-box-icons-images)
  (setq company-box-scrollbar t)
  (setq company-box-doc-enable nil)
  (setq company-tooltip-minimum-width 80)
  (setq company-tooltip-maximum-width 80))

(use-package company-quickhelp
  :straight t
  :after company
  :init
  (setq company-quickhelp-delay nil))

(provide 'config-company)
