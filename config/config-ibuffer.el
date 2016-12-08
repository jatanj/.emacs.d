(general-define-key
 :keymaps 'ibuffer-mode-map
 "C-d" (general-simulate-keys "<next>")
 "C-u" (general-simulate-keys "<prior>"))

(provide 'config-ibuffer)
