(use-package ibuffer-projectile
  :straight t
  :after projectile
  :init
  (add-hook 'ibuffer-hook
    (lambda ()
      (setq ibuffer-display-summary nil)
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))
      (centaur-tabs-local-disable)
      (centaur-tabs-blend-header-line "Buffers")))
  :config
  (general-define-key
   :keymaps 'ibuffer-mode-map
   "C-d" (general-simulate-key "<next>")
   "C-u" (general-simulate-key "<prior>")))

(use-package all-the-icons-ibuffer
  :straight t
  :init
  (all-the-icons-ibuffer-mode 1))

(provide 'config-ibuffer-projectile)
