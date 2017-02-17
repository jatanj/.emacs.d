(require 'magit)
(require 'evil-magit)

(magit-auto-revert-mode -1) ; We already use global-auto-revert-mode
(evil-set-initial-state 'magit-mode 'emacs)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(global-git-commit-mode)
(add-hook 'git-commit-mode-hook 'flyspell-mode)
(add-hook 'git-commit-mode-hook (lambda () (toggle-save-place 0)))
(setq vc-follow-symlinks t)

;; Disable tabbar-mode in magit popups
(add-hook 'magit-popup-mode-hook
 (lambda ()
   (when (bound-and-true-p tabbar-mode)
     (tabbar-local-mode))))

(general-define-key
 :states '(magit normal visual)
 :keymaps 'magit-mode-map
 "C-k" nil
 "C-<tab>" (general-simulate-keys "^ <tab>")
 "C-S-<tab>" 'magit-section-cycle-diffs
 "S-<up>" (lambda () (interactive) (evil-previous-line 10))
 "S-<down>" (lambda () (interactive) (evil-next-line 10))
 "S-<left>" (lambda () (interactive) (evil-backward-char 10))
 "S-<right>" (lambda () (interactive) (evil-forward-char 10)))

(provide 'config-magit)
