(use-package ibuffer-projectile
  :ensure t
  :after projectile
  :init
  (add-hook 'ibuffer-hook
    (lambda ()
      (setq ibuffer-display-summary nil)
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))
      (tabbar-blend-header-line
       (format "Buffers [%d files, %d total]"
               (length (file-list))
               (length (buffer-list))))))
  :config
  (general-define-key
   :keymaps 'ibuffer-mode-map
   "C-d" (general-simulate-key "<next>")
   "C-u" (general-simulate-key "<prior>")))

(provide 'config-ibuffer-projectile)
