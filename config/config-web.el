(require 'web-mode)

(dolist (assoc '(("\\.phtml\\'"     . web-mode)
                 ("\\.tpl\\.php\\'" . web-mode)
                 ("\\.[agj]sp\\'"   . web-mode)
                 ("\\.as[cp]x\\'"   . web-mode)
                 ("\\.erb\\'"       . web-mode)
                 ("\\.mustache\\'"  . web-mode)
                 ("\\.djhtml\\'"    . web-mode)
                 ("\\.html?\\'"     . web-mode)
                 ("\\.js\\'"        . js2-mode)
                 ("\\.jsx?\\'"      . js2-jsx-mode)
                 ("node"            . js2-jsx-mode)))
  (add-to-list 'auto-mode-alist assoc))

(setq js-indent-level 2)
(setq javascript-indent-level 2)
(setq js2-basic-offset 2)
(setq js3-indent-level 2)

(setq js2-mode-assume-strict t)
(setq js2-mode-show-parse-errors nil)
(setq js2-idle-timer-delay 0.2)

(sp-local-pair 'js2-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(setq sgml-basic-offset 2)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)

(provide 'config-web)
