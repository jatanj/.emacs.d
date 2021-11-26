(dolist (assoc '(("\\.sh\\'" . sh-mode)
                 ("PKGBUILD" . sh-mode)))
  (add-to-list 'auto-mode-alist assoc))

(use-package xterm-color
  :straight t
  :init
  (add-hook 'shell-mode-hook
    (lambda ()
      ;; Disable font-locking in this buffer to improve performance
      (font-lock-mode -1)
      ;; Prevent font-locking from being re-enabled in this buffer
      (make-local-variable 'font-lock-function)
      (setq font-lock-function (lambda (_) nil))
      (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (setq xterm-color-names
    ["#434C5E"
     "#BF616A"
     "#A3BE8C"
     "#EBCB8B"
     "#81A1C1"
     "#7272A1"
     "#3F8BAA"
     "#E5E9F0"])
  (setq xterm-color-names-bright
    ["#5D6678"
     "#BF616A"
     "#A3BE8C"
     "#EBCB8B"
     "#81A1C1"
     "#8989C4"
     "#5AA4C2"
     "#ECEFF4"]))

(add-hook 'sh-mode-hook
          (lambda ()
            (flycheck-mode)
            (config/set-local-tab-width 4)))

(general-define-key
 :keymaps 'shell-mode-map
 "C-c C-l" 'erase-buffer)

(provide 'config-shell)
