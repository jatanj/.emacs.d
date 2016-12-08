(setq scala-indent:use-javadoc-style t)
(sp-local-pair 'scala-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(setq ensime-startup-notification nil)
(setq ensime-startup-snapshot-notification nil)

(setq ensime-implicit-gutter-icons nil)
(setq ensime-left-margin-gutter nil)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*ENSIME-Compilation-Result*" eos)
               (display-buffer-reuse-window
                display-buffer-below-selected)
               (window-height   . 0.20)))

(general-define-key
 :keymaps 'ensime-mode-map
 "<f12>" 'ensime-edit-definition
 "C-<f12>" 'ensime-pop-find-definition-stack
 "C-c C-c C-c" 'ensime-print-errors-at-point)

(provide 'config-scala)
