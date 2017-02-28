(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
  (ignore-errors (require 'livedown))
  (setq markdown-gfm-use-electric-backquote nil)
  (dolist (mode '(gfm-mode markdown-mode))
    (add-hook (intern (format "%s-hook" mode)) #'flyspell-mode))
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
   "M-<right>" nil))

(provide 'config-markdown)
