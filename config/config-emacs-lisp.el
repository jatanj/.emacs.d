(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (company-mode 1)
    (set-local-tab-width 2)))

(general-define-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 :prefix "C-c"
 "C-e" 'eval-last-sexp
 "C-r" 'eval-region
 "C-c" 'eval-defun
 "C-f" 'eval-defun
 "C-k" 'eval-buffer)

(provide 'config-emacs-lisp)
