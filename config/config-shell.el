(dolist (assoc '(("\\.sh\\'" . sh-mode)
                 ("PKGBUILD" . sh-mode)))
  (add-to-list 'auto-mode-alist assoc))

(use-package xterm-color
  :straight t
  :init
  (setq comint-output-filter-functions
        (remove
         'ansi-color-process-output
         comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              (add-hook
               'comint-preoutput-filter-functions
               'xterm-color-filter nil t))))

(add-hook 'sh-mode-hook
          (lambda ()
            (flycheck-mode)
            (config/set-local-tab-width 4)))

(provide 'config-shell)
