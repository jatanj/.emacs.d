(use-package fsharp-mode
  :after smartparens
  :config
  (setq fsharp-ac-use-popup t)
  (setq fsharp-indent-offset 2)
  (setq inferior-fsharp-program "fsharpi --readline- --shadowcopyreferences+")

  (add-hook 'fsharp-mode-hook
            (lambda ()
              (setq indent-line-function 'indent-relative-maybe)
              (set-local-tab-width 2)
              (setq company-auto-complete nil)))

  (sp-local-pair #'fsharp-mode "'" nil :actions nil)

  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(rx bos "*inferior-fsharp*" eos)
  ;;                (display-buffer-reuse-window
  ;;                 display-buffer-below-selected)
  ;;                (window-height   . 0.20)))
  ;; (evil-set-initial-state 'inferior-fsharp-mode 'emacs)

  (general-define-key
   :keymaps 'fsharp-mode-map
   "C-c C-k" 'fsharp-load-buffer-file
   "C-c C-n" 'fsharp-send-references))

(provide 'config-fsharp)
