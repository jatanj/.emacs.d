(use-package quickrun
  :ensure t
  :init
  (setq quickrun-focus-p nil)
  (setq quickrun-timeout-seconds nil)
  :config
  (general-define-key
   :keymaps '(prog-mode-map)
   :prefix "C-c"
   "C-q C-k" 'quickrun
   "C-q C-a" 'quickrun-with-arg
   "C-q C-q" 'quickrun-region
   "C-q C-a" 'quickrun-replace-region))

(provide 'config-quickrun)
