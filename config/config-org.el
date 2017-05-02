(use-package org
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode 1)
              (org-bullets-mode 1)
              (org-toggle-pretty-entities)))
  (add-hook 'text-mode-hook #'turn-on-orgtbl)
  :config
  (require 'org-mouse)
  (setq org-hide-emphasis-markers t)
  (setq-default org-startup-folded 'showall)
  (defhydra hydra-org-move (org-mode-map "C-c")
    ("<up>" org-metaup)
    ("<down>" org-metadown))
  (defhydra hydra-org-rank (org-mode-map "C-c")
    ("<left>" org-metaleft)
    ("<right>" org-metaright))
  (defhydra hydra-org-insert-row (org-mode-map "C-c")
    ("<return>" org-table-insert-row "Insert row"))
  (general-define-key
   :keymaps 'org-mode-map
   "<tab>" 'org-cycle
   "<return>" 'org-return
   "C-k" nil
   "S-<return>" 'org-insert-heading
   "M-<up>" nil
   "M-<down>" nil
   "M-<left>" nil
   "M-<right>" nil))

(use-package org-bullets
  :ensure t
  :after org
  :init
  (setq org-ellipsis " …")
  (setq org-bullets-bullet-list '("⚫")))

(provide 'config-org)
