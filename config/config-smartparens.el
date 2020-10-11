(use-package smartparens
  :straight t
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
                  rust-mode
                  typescript-mode
                  js-mode
                  js2-mode
                  css-mode
                  scss-mode))
    (sp-local-pair mode "{" nil :post-handlers '((newline-and-enter-sexp "RET"))))

  (dolist (mode '(fsharp-mode
                  scala-mode))
    (sp-local-pair mode "'" nil :actions nil))

  (dolist (mode '(gfm-mode
                  markdown-mode))
    (sp-local-pair mode "`" nil :actions nil)
    (sp-local-pair mode "'" nil :actions nil)
    (sp-local-pair mode "\"" nil :actions nil))

  (defun config/sp-point-before-all-pairs-p (_id action _context)
    (when (eq action 'insert)
      (sp--looking-at-p "[\\[\\(\\{]")))

  (sp-pair "(" nil :unless '(sp-in-string-p sp-point-before-same-p sp-point-before-word-p config/sp-point-before-all-pairs-p))
  (sp-pair "[" nil :unless '(sp-in-string-p sp-point-before-same-p sp-point-before-word-p config/sp-point-before-all-pairs-p))
  (sp-pair "{" nil :unless '(sp-in-string-p sp-point-before-same-p sp-point-before-word-p config/sp-point-before-all-pairs-p))

  (defhydra hydra-smartparens-sexp (smartparens-mode-map "C-c")
    ("]" sp-up-sexp "Up sexp")
    ("[" sp-down-sexp "Down sexp")))

(defun newline-and-enter-sexp (&rest _)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(provide 'config-smartparens)
