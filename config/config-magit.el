(require 'magit)
(require 'evil-magit)

;; https://github.com/magit/magit/issues/2541
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))

(general-define-key
 :keymaps 'magit-mode-map
  "C-k" (general-simulate-keys "C-x")
  "SPC" (general-simulate-keys "M-x" t)
  "M-<f4>" (if (daemonp) 'delete-frame 'save-buffers-kill-emacs)
  "C-:" 'eval-expression
  "C-<tab>" 'previous-buffer
  "C-<prior>" 'tabbar-backward-tab
  "C-<next>" 'tabbar-forward-tab)

(provide 'config-magit)
