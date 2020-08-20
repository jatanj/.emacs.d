(use-package fsharp-mode
  :defer t
  :mode ("\\.fs[ix]?\\'" . fsharp-mode)
  :init
  (add-hook 'fsharp-mode-hook
    (lambda ()
      ;; (setq indent-line-function 'indent-relative-maybe)
      (setq indent-region-function nil)
      (config/set-local-tab-width 2)
      (setq company-auto-complete nil)))
  :config
  (setq fsharp-ac-use-popup t)
  (setq fsharp-indent-level 2)
  (setq fsharp-indent-offset 2)
  (setq fsharp-continuation-offset 2)
  (setq inferior-fsharp-program "fsharpi --readline- --shadowcopyreferences+")
  (general-define-key
   :keymaps 'fsharp-mode-map
   "C-c C-k" 'fsharp-load-buffer-file
   "C-c C-n" 'fsharp-send-references))

(provide 'config-fsharp)
