(setq evil-want-C-u-scroll t)
(setq evil-toggle-key "<f5>")

(require 'evil)

(evil-mode 1)

(setq evil-disable-insert-state-bindings t)
(setq-default evil-shift-width 2)

(evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'Buffer-menu-mode 'emacs)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-visualstar)
(global-evil-visualstar-mode 1)

(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; Improve shift to keep selection
;; http://superuser.com/questions/684540/#answer-789156
(defun evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))
(defun evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(provide 'config-evil)
