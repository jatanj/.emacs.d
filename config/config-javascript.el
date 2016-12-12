(dolist (assoc '(("\\.js\\'"        . js2-mode)
                 ("\\.jsx?\\'"      . web-mode)
                 ("node"            . js2-jsx-mode)))
  (add-to-list 'auto-mode-alist assoc))

(setq js-indent-level 2)
(setq javascript-indent-level 2)
(setq js2-basic-offset 2)
(setq js3-indent-level 2)

(setq js2-mode-assume-strict t)
(setq js2-mode-show-parse-errors nil)
(setq js2-idle-timer-delay 0.2)

(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook
  (lambda ()
    (when (string-equal "jsx" (file-name-extension buffer-file-name))
      (setup-tide-mode))))

(sp-local-pair 'js2-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(provide 'config-javascript)
