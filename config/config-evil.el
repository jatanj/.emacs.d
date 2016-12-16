(setq evil-want-C-u-scroll t)
(setq evil-intercept-esc nil)
(setq evil-toggle-key "<f5>")

(evil-mode 1)

(setq evil-disable-insert-state-bindings t)
(setq-default evil-shift-width 2)

(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'Buffer-menu-mode 'emacs)

(global-evil-surround-mode 1)
(global-evil-visualstar-mode 1)
(global-evil-matchit-mode 1)

(general-define-key
 :states 'normal
 "=" 'er/expand-region
 "q" 'ignore)

(general-define-key
 :states 'insert
 "<tab>" 'tab-to-tab-stop
 "C-k" ctl-x-map
 "C-g" 'evil-force-normal-state
 "C-c" 'kill-ring-save
 "C-x" 'kill-region
 "C-v" 'yank)

(general-define-key
 :states 'visual
 ">" 'evil-shift-right-visual
 "<" 'evil-shift-left-visual
 "C-[" 'evil-shift-left-visual
 "C-]" 'evil-shift-right-visual
 "<tab>" 'evil-shift-right-visual
 "C-S-<tab>" 'evil-shift-left-visual)

(general-define-key
 :states '(normal insert)
 "C-[" 'evil-shift-left-line
 "C-]" 'evil-shift-right-line)

(general-define-key
 :states '(normal visual)
 "SPC" (general-simulate-keys "C-k"))

(general-define-key
 :states '(normal insert visual)
 "C-z" 'undo-tree-undo
 "C-s" 'save-buffer
 "C-f" 'isearch-forward-regexp
 "C-S-f" 'isearch-backward-regexp
 "C-h" 'query-replace-regexp
 "C-S-h" 'anzu-query-replace-at-cursor-thing
 "C-b" 'do-nothing)

(general-define-key
 :states '(normal insert visual motion)
 "C-q" (lambda () (interactive) (scroll-down 1))
 "S-<up>" (lambda () (interactive) (evil-previous-line 10))
 "S-<down>" (lambda () (interactive) (evil-next-line 10)))

(general-define-key
 :states '(normal visual emacs motion))

(general-define-key
 :states '(normal insert visual emacs motion)
 "C-/" 'comment-line-or-region
 "<home>" 'back-to-indentation
 "C-_" 'shrink-window
 "C-S-p" 'helm-M-x
 "C-p" 'helm-projectile-find-file)

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
