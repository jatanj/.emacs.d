(use-package haskell-mode
  :straight t
  :defer t
  :mode ("\\.l?hs\\'" . haskell-mode)
  :init
  (add-hook 'haskell-mode-hook
    (lambda ()
      (interactive-haskell-mode)
      (company-mode 1)
      (flycheck-mode 1)
      (setq flycheck-disabled-checkers '(haskell-stack-ghc))))
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (setq haskell-process-type 'cabal-repl)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)
  (general-define-key
   :keymaps 'haskell-mode-map
   "C-c C-j" 'haskell-interactive-bring
   "C-c C-d" 'haskell-mode-jump-to-def-or-tag))

(use-package flycheck-haskell
  :straight t
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'config-haskell)
