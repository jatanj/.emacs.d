(add-hook 'd-mode-hook
  (lambda ()
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (set-local-tab-width 3)
    (set (make-local-variable 'c-basic-offset) 3)
    (set (make-local-variable 'c-indent-level) 3)))

(sp-local-pair 'd-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(provide 'config-d)
