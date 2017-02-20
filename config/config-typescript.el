(use-package tide
  :after smartparens-config
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (setq company-tooltip-align-annotations t)
  (setq typescript-indent-level 2)
  (setq tide-format-options
        '(:indentSize 2
                      :tabSize 2
                      :convertTabsToSpaces t))

  (add-to-list 'auto-mode-alist '("\\.d\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  (sp-local-pair 'typescript-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Typescript*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height   . 0.20))))

(use-package ts-comint
  :after tide
  :config
  (general-define-key
   :keymaps 'typescript-mode-map
   "C-c C-d" 'tide-jump-to-definition
   "C-c C-e" 'ts-send-last-sexp
   "C-c C-k" 'ts-send-buffer
   "C-c C-l" 'ts-load-file-and-go))

(provide 'config-typescript)
