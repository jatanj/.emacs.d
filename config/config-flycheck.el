(defun flycheck-toggle-fix ()
  (interactive)
  (flycheck-mode 'toggle)
  (flycheck-mode 'toggle))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.125)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck error messages*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.125)))

(evil-set-initial-state 'flycheck-error-list-mode 'emacs)

(general-define-key
 :keymaps 'flycheck-mode-map
 "C-c ! f" 'flycheck-toggle-fix
 "C-c <C-up>" 'flycheck-previous-error
 "C-c <C-down>" 'flycheck-next-error)

(provide 'config-flycheck)
