(use-package ensime
  :ensure t
  :defer t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :init
  (setq scala-indent:use-javadoc-style t)
  :config
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  (setq ensime-implicit-gutter-icons nil)
  (setq ensime-left-margin-gutter nil)
  (setq ensime-sbt-perform-on-save "compile")
  (setq ensime-typecheck-interval 1)
  (setq ensime-typecheck-idle-interval 0.2)
  (setq ensime-use-helm t)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*ENSIME-Compilation-Result*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height   . 0.20)))
  (general-define-key
   :keymaps 'ensime-mode-map
   "C-c C-]" 'ensime-edit-definition
   "C-c C-k" 'ensime-inf-eval-buffer
   "C-c C-s" 'ensime-print-errors-at-point))

(provide 'config-scala)
