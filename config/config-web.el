(require 'web-mode)

(dolist (assoc '(("\\.phtml\\'"     . web-mode)
                 ("\\.tpl\\.php\\'" . web-mode)
                 ("\\.[agj]sp\\'"   . web-mode)
                 ("\\.as[cp]x\\'"   . web-mode)
                 ("\\.erb\\'"       . web-mode)
                 ("\\.mustache\\'"  . web-mode)
                 ("\\.djhtml\\'"    . web-mode)
                 ("\\.html?\\'"     . web-mode)))
  (add-to-list 'auto-mode-alist assoc))

(setq sgml-basic-offset 2)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)

(provide 'config-web)
