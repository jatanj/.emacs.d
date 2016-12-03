(setq scala-indent:use-javadoc-style t)
(sp-local-pair 'scala-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(provide 'config-scala)
