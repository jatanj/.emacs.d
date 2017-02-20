(use-package smartparens
  :config
  (require 'smartparens-config)

  (smartparens-global-mode 1)

  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  (setq sp-escape-quotes-after-insert nil)
  (setq sp-escape-wrapped-region nil)

  (setq sp-autoinsert-quote-if-followed-by-closing-pair nil)

  (dolist (mode '(c-mode c++-mode java-mode))
    (sp-local-pair mode "{" nil :post-handlers '((newline-and-enter-sexp "RET"))))

  (sp-pair "(" nil :unless '(sp-point-before-same-p sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-point-before-same-p sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-point-before-same-p sp-point-before-word-p)))

(provide 'config-smartparens)
