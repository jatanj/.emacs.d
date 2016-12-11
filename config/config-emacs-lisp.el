(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (company-mode 1)
    (set-local-tab-width 2)))

(provide 'config-emacs-lisp)
