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
         ("\\.djhtml\\'"    . web-mode)
         ("\\.html?\\'"     . web-mode)
         ("\\.jsx\\'"       . web-mode)
         ("\\.tsx\\'"       . web-mode))
  :config
  (setq sgml-basic-offset 2)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq css-indent-offset 2)
  (add-hook 'web-mode-hook
    (lambda ()
      (let ((ext (file-name-extension (buffer-file-name))))
        (when (or (string= ext "jsx")
                  (string= ext "tsx"))
          (setup-tide-mode))))))

(provide 'config-web-mode)
