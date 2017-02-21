(use-package tide
  :ensure t
  :defer t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.d\\.ts\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2)
  (setq tide-format-options '(:indentSize 2
                              :tabSize 2
                              :convertTabsToSpaces t))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Typescript*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height   . 0.20))))

(defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (company-mode 1)
    (flycheck-mode 1)
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1))

(use-package ts-comint
  :ensure t
  :after tide
  :config
  (general-define-key
   :keymaps 'typescript-mode-map
   "C-c C-d" 'tide-jump-to-definition
   "C-c C-e" 'ts-send-last-sexp
   "C-c C-k" 'ts-send-buffer
   "C-c C-l" 'ts-load-file-and-go))

(provide 'config-typescript)
