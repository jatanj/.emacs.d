(let ((term-mode-keybindings `("C-k" ,ctl-x-map)))
  (apply 'general-define-key (append '(:keymaps term-mode-map) term-mode-keybindings))
  (apply 'general-define-key (append '(:keymaps term-raw-map) term-mode-keybindings)))

(provide 'config-term)
