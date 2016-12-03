(require 'smartparens-config)

(smartparens-global-mode 1)

(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

(setq sp-escape-quotes-after-insert nil)
(setq sp-escape-wrapped-region nil)

(setq sp-autoinsert-quote-if-followed-by-closing-pair nil)

(sp-pair "(" nil :unless '(sp-point-before-word-p))
(sp-pair "[" nil :unless '(sp-point-before-word-p))
(sp-pair "{" nil :unless '(sp-point-before-word-p))

(provide 'config-smartparens)
