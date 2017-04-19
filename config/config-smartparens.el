(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (setq sp-escape-quotes-after-insert nil)
  (setq sp-escape-wrapped-region nil)
  (setq sp-autoinsert-quote-if-followed-by-closing-pair nil)
  (sp-pair "\\\"" nil :actions :rem)
  (dolist (mode '(c-mode
                  c++-mode
                  java-mode
                  groovy-mode
                  scala-mode
                  d-mode
                  typescript-mode
                  js2-mode
                  rust-mode))
    (sp-local-pair mode "{" nil :post-handlers '((newline-and-enter-sexp "RET"))))
  (sp-local-pair #'fsharp-mode "'" nil :actions nil)
  (dolist (mode '(gfm-mode
                  markdown-mode))
    (sp-local-pair mode "`" nil :actions nil)
    (sp-local-pair mode "'" nil :actions nil)
    (sp-local-pair mode "\"" nil :actions nil))
  (sp-pair "(" nil :unless '(sp-in-string-p sp-point-before-same-p sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-in-string-p sp-point-before-same-p sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-in-string-p sp-point-before-same-p sp-point-before-word-p))
  (defhydra hydra-smartparens-sexp (smartparens-mode-map "C-c")
    ("]" sp-up-sexp "Up sexp")
    ("[" sp-down-sexp "Down sexp")))

(defun newline-and-enter-sexp (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(provide 'config-smartparens)
