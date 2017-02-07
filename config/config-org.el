(require 'org-mouse)

(use-package org-bullets
  :init
  (setq org-ellipsis " …")
  (setq org-bullets-bullet-list '("⚫")))

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
  ("<down>" org-metadown))

(defhydra hydra-org-rank (org-mode-map "C-c")
  ("<left>" org-metaleft)
  ("<right>" org-metaright))

(general-define-key
 :keymaps 'org-mode-map
 "<tab>" 'org-cycle
 "<return>" 'org-return
 "C-k" nil
 "S-<return>" 'org-insert-heading
 "M-<up>" nil
 "M-<down>" nil
 "M-<left>" nil
 "M-<right>" nil)

(provide 'config-org)
