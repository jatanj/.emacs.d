(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)

(setq haskell-process-type 'cabal-repl)
(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)

(add-hook 'haskell-mode-hook
          (lambda ()
            (interactive-haskell-mode)
            (company-mode 1)
            (flycheck-mode 1)
            (flycheck-haskell-setup)
            (setq flycheck-disabled-checkers '(haskell-stack-ghc))))

(general-define-key
 :keymaps 'haskell-mode-map
 "C-c C-j" 'haskell-interactive-bring
 "C-]" 'haskell-mode-jump-to-def-or-tag)

(provide 'config-haskell)