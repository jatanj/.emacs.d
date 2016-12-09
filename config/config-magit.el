(require 'magit)

(evil-set-initial-state 'magit-mode 'emacs)

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
 :prefix leader-key
 "v" 'magit-status)

(provide 'config-magit)
