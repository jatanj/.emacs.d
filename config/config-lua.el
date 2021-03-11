(use-package lua-mode
  :straight t
  :init
  (config/set-local-tab-width 2)
  (setq lua-indent-level 2)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil)

  (defun config/lua-mode-init ()
    (smartparens-mode -1))
  (add-hook 'lua-mode-hook #'config/lua-mode-init)

  :config
  ;; https://github.com/immerrr/lua-mode/issues/132#issuecomment-458930991
  (defun config/lua-at-most-one-indent (old-function &rest arguments)
    (let ((old-res (apply old-function arguments)))
      (if (> old-res 2) 2 old-res)))
  (advice-add #'lua-calculate-indentation-block-modifier :around #'config/lua-at-most-one-indent))

(provide 'config-lua)
