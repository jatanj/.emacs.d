(use-package fsharp-mode
  :ensure t
  :defer t
  :mode ("\\.fs[ix]?\\'" . fsharp-mode)
  :config
  (setq fsharp-ac-use-popup t)
  (setq fsharp-indent-offset 2)
  (setq inferior-fsharp-program "fsharpi --readline- --shadowcopyreferences+")
  (add-hook 'fsharp-mode-hook
    (lambda ()
      (setq indent-line-function 'indent-relative-maybe)
      (set-local-tab-width 2)
      (setq company-auto-complete nil)))
  (general-define-key
   :keymaps 'fsharp-mode-map
   "C-c C-k" 'fsharp-load-buffer-file
   "C-c C-n" 'fsharp-send-references))

(provide 'config-fsharp)
