(setq scala-indent:use-javadoc-style t)
(setq scala-enable-eldoc t)

(setq ensime-startup-notification nil)
(setq ensime-startup-snapshot-notification nil)
(setq ensime-implicit-gutter-icons nil)
(setq ensime-left-margin-gutter nil)

(sp-local-pair 'scala-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*ENSIME-Compilation-Result*" eos)
               (display-buffer-reuse-window
                display-buffer-below-selected)
               (window-height   . 0.20)))

(general-define-key
 :keymaps 'ensime-mode-map
 "C-c C-e" 'ensime-edit-definition
 "C-c C-s" 'ensime-print-errors-at-point)

(provide 'config-scala)
