(use-package anzu
  :config
  (global-anzu-mode +1)

  (set-face-attribute 'anzu-mode-line nil :foreground "#639743" :weight 'normal)

  (global-set-key [remap isearch-query-replace] 'anzu-isearch-query-replace)
  (global-set-key [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(provide 'config-anzu)
