(setq c-basic-offset 4)
(setq c-indent-level 4)
(setq-default c-default-style "k&r")

(defun customize-cc-mode ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (set-local-tab-width 4))

(add-hook 'c-mode-common-hook 'customize-cc-mode)

(defun newline-and-enter-sexp (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))
(sp-local-pair 'c-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))
(sp-local-pair 'c++-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(provide 'config-c-cpp)
