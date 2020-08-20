(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (company-mode 1)
    (eldoc-mode -1)
    (config/set-local-tab-width 2)))

;; https://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(general-define-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 :prefix "C-c"
 "C-e" 'eval-last-sexp
 "C-r" 'eval-region
 "C-c" 'eval-defun
 "C-f" 'eval-defun
 "C-k" 'eval-buffer
 "C-a" 'eval-and-replace)

(provide 'config-emacs-lisp)
