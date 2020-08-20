(use-package nxml-mode
  :init
  (defun xml-pretty-print ()
    (interactive)
    (shell-command-replace-region "xmllint --format -"))
  (defun xml-escape ()
    (interactive)
    (web-mode-dom-xml-replace))
  (defun xml-unescape ()
    (interactive)
    (web-mode-dom-entities-replace))
  (defun xml-no-blanks ()
    (interactive)
    (shell-command-replace-region "xmllint --noblanks -"))
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq nxml-child-indent 4)
              (config/set-local-tab-width 4)))
  :config
  (general-define-key
   :keymaps 'nxml-mode-map
   "C-c C-f" 'xml-pretty-print
   "C-c C-w" 'xml-escape
   "C-c C-q" 'xml-unescape))

(provide 'config-xml)
