(require 'fsharp-mode)

;; (setq fsharp-ac-use-popup t)
(setq fsharp-indent-offset 2)

(add-hook 'fsharp-mode-hook
  (lambda ()
    (setq indent-line-function 'indent-relative-maybe)
    (set-local-tab-width 2)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*inferior-fsharp*" eos)
               (display-buffer-reuse-window
                display-buffer-below-selected)
               (window-height   . 0.20)))
(evil-set-initial-state 'inferior-fsharp-mode 'emacs)

(general-define-key
 :keymaps 'fsharp-mode-map
 "C-]" 'fsharp-ac/gotodefn-at-point
 "C-o" 'fsharp-ac/pop-gotodefn-stack
 "C-c C-d" 'ignore)

(provide 'config-fsharp)
