(magit-auto-revert-mode -1) ; We already use global-auto-revert-mode
(evil-set-initial-state 'magit-mode 'emacs)

(add-hook 'git-commit-mode-hook 'flyspell-mode)
(add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))

;; Always follow symlinks for Git-controlled files
(setq vc-follow-symlinks t)

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

(provide 'config-magit)
