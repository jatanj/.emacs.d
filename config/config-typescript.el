(require 'tide)
(require 'ts-comint)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (company-mode 1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq company-tooltip-align-annotations t)

(setq typescript-indent-level 2)
(setq tide-format-options
  '(:indentSize 2
    :tabSize 2
    :convertTabsToSpaces t))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
  (lambda ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode))))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Typescript*" eos)
               (display-buffer-reuse-window
                display-buffer-below-selected)
               (window-height   . 0.20)))

(general-define-key
 :keymaps 'typescript-mode-map
 "<f12>" 'tide-jump-to-definition
 "C-<f12>" 'tide-jump-back
 "C-c C-e" 'ts-send-last-sexp
 "C-c C-k" 'ts-send-buffer
 "C-c C-l" 'ts-load-file-and-go)

(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(sp-local-pair 'typescript-mode "{" nil :post-handlers '((newline-and-enter-sexp "RET")))

(provide 'config-typescript)
