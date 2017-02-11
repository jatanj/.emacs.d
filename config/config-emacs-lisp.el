(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (company-mode 1)
    (set-local-tab-width 2)))

(general-define-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "C-c C-e" 'eval-last-sexp
 "C-c C-r" 'eval-region
 "C-c C-f" 'eval-defun
 "C-c C-k" 'eval-buffer)

(provide 'config-emacs-lisp)
