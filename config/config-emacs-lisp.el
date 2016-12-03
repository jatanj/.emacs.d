(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (company-mode -1)
    (set-local-tab-width 2)))

(add-hook 'emacs-lisp-mode-hook #'company-mode)

(provide 'config-emacs-lisp)
