(use-package nxml-mode
  :init
  (defun xml-reformat ()
    (interactive)
    (shell-command-replace-region "xmllint --format -"))
  (defun xml-unformat ()
    (interactive)
    (shell-command-replace-region "xmllint --noblanks -"))
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq nxml-child-indent 4)
              (set-local-tab-width 4)))
  :config
  (general-define-key
   :keymaps 'nxml-mode-map
   "C-c C-f" 'xml-reformat))

(provide 'config-xml)
