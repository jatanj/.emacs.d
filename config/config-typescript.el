(use-package typescript-mode
  :straight t
  :mode (("\\(\\.d\\)?\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-tsx-mode))
  :init
  (setq typescript-indent-level 2)

  (defun config/typescript-mode-init ()
    (setq company-minimum-prefix-length 0)
    (eldoc-mode -1)
    (add-hook 'config/save-keybind-hook 'config/eslint-after-save nil t))
  (add-hook 'typescript-mode-hook #'config/typescript-mode-init)

  (defun config/typescript-is-tsx-file-p (file-name)
    (string= (file-name-extension file-name) "tsx"))

  (defun config/typescript-web-mode-init ()
    (when (config/typescript-is-tsx-file-p (buffer-file-name))
      (config/typescript-mode-init)))
  (add-hook 'web-mode-hook #'config/typescript-web-mode-init)

  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Typescript*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height . 0.20))))

(use-package tree-sitter
  :straight t
  :hook ((typescript-mode . tree-sitter-hl-mode)
	 (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package ts-comint
  :straight t
  :after tide
  :config
  (general-define-key
   :keymaps 'typescript-mode-map
   "C-c C-d" 'tide-jump-to-definition
   "C-c C-e" 'ts-send-last-sexp
   "C-c C-k" 'ts-send-buffer
   "C-c C-l" 'ts-load-file-and-go))

(provide 'config-typescript)
