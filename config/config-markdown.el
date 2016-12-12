(dolist (assoc '(("README\\.md\\'" . gfm-mode)
                 ("\\.md\\'"       . markdown-mode)
                 ("\\.markdown\\'" . markdown-mode)))
  (add-to-list 'auto-mode-alist assoc))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(ignore-errors (require 'livedown))

(general-define-key
 :keymaps '(markdown-mode-map gfm-mode-map)
 "C-c C-k" 'livedown-preview
 "C-c C-q" 'livedown-kill
 "C-c C-<up>" 'markdown-move-up
 "C-c C-<down>" 'markdown-move-down
 "C-c C-<left>" 'markdown-promote
 "C-c C-<right>" 'markdown-demote
 "M-<up>" nil
 "M-<down>" nil
 "M-<left>" nil
 "M-<right>" nil)

(provide 'config-markdown)
