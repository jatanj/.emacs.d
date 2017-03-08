(use-package ibuffer-projectile
  :ensure t
  :after projectile
  :init
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))
  :config
  (general-define-key
   :keymaps 'ibuffer-mode-map
   "C-d" (general-simulate-keys "<next>")
   "C-u" (general-simulate-keys "<prior>")))

(provide 'config-ibuffer-projectile)
