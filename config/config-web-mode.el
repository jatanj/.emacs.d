(use-package web-mode
  :ensure t
  :defer t
  :after tide
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.html?\\'"     . web-mode)
         ("\\.tsx\\'"       . web-mode))
  :init
  (add-hook 'web-mode-hook
    (lambda ()
      (when (string= (file-name-extension (buffer-file-name)) "tsx")
        (setup-tide-mode))))
  :config
  (setq sgml-basic-offset 2)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq css-indent-offset 2))

(provide 'config-web-mode)
