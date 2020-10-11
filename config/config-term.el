(let ((term-mode-keybindings `("C-k" ,ctl-x-map)))
  (apply 'general-define-key (append '(:keymaps term-mode-map) term-mode-keybindings))
  (apply 'general-define-key (append '(:keymaps term-raw-map) term-mode-keybindings)))

(defun config/term-line-mode ()
  (interactive)
  (evil-normal-state)
  (term-line-mode))

(defun config/term-char-mode ()
  (interactive)
  (evil-emacs-state)
  (term-char-mode))

(general-define-key
 :keymaps 'term-mode-map
 "C-c C-z" nil)

(general-define-key
 :keymaps 'term-raw-map
 "C-c C-j" 'config/term-line-mode
 "C-c C-k" 'config/term-char-mode)

(provide 'config-term)
