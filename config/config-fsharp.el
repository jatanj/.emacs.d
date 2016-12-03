(setq fsharp-ac-use-popup t)
(setq fsharp-indent-offset 2)

(add-hook 'fsharp-mode-hook
  (lambda ()
    (setq indent-line-function 'indent-relative-maybe)
    (set-local-tab-width 2)
    (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)))

(provide 'config-fsharp)
