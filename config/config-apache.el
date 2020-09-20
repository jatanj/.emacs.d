(use-package apache-mode
  :straight t
  :mode (("\\.htaccess\\'"   . apache-mode)
         ("httpd\\.conf\\'"  . apache-mode)
         ("srm\\.conf\\'"    . apache-mode)
         ("access\\.conf\\'" . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

(provide 'config-apache)
