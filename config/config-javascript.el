(use-package js2-mode
  :straight t
  :defer t
  :after tide
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :init
  (setq js-indent-level 2)
  (setq javascript-indent-level 2)
  (add-hook 'js2-mode-hook
            (lambda ()
              (flycheck-mode 1)
              (c-set-offset 'substatement-open 0)
              (c-set-offset 'case-label '+)
              (c-set-offset 'arglist-intro '+)))
  :config
  (setq js2-basic-offset 2)
  (setq js2-mode-assume-strict t)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-idle-timer-delay 0.2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package js2-refactor
  :straight t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (add-hook 'js2-mode-hook #'js2-refactor-mode))

(provide 'config-javascript)
