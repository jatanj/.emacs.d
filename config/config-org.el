(require 'org-mouse)

(use-package org-bullets
  :init
  (setq org-ellipsis " …")
  (setq org-bullets-bullet-list '("•")))

(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'content)

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)
            (org-bullets-mode 1)))

(font-lock-add-keywords 'org-mode
  			  '(("^ +\\([-*]\\) "
  			     (0 (prog1 ()
                  (compose-region (match-beginning 1) (match-end 1) "•"))))))

(general-define-key
 :keymaps 'org-mode-map
 "C-k" nil
 "S-<return>" 'org-insert-heading
 "M-<up>" nil
 "M-<down>" nil
 "M-<left>" nil
 "M-<right>" nil
 "C-c C-<up>" 'org-metaup
 "C-c C-<down>" 'org-metadown
 "C-c C-<left>" 'org-metaleft
 "C-c C-<right>" 'org-metaright)

(provide 'config-org)
