(use-package org
  :after org-mouse
  :config
  (setq org-hide-emphasis-markers t)
  (setq-default org-startup-folded 'showall)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-indent-mode 1)
              (org-bullets-mode 1)))
  (font-lock-add-keywords 'org-mode
                          '(("^ +\\([-*]\\) "
                             (0 (prog1 ()
                                  (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (defhydra hydra-org-move (org-mode-map "C-c")
    ("<up>" org-metaup)
    ("C-<up>" org-metup)
    ("<down>" org-metadown)
    ("C-<down>" org-metadown))
  (defhydra hydra-org-rank (org-mode-map "C-c")
    ("<left>" org-metaleft)
    ("C-<left>" org-metaleft)
    ("<right>" org-metaright)
    ("C-<right>" org-metaright))
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
