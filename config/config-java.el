(add-hook 'java-mode-hook 'customize-cc-mode)
(sp-local-pair 'java-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(provide 'config-java)
