(use-package fsharp-mode
  :ensure t
  :defer t
  :mode ("\\.fs[ix]?\\'" . fsharp-mode)
  :init
  (add-hook 'fsharp-mode-hook
    (lambda ()
      ;; (setq indent-line-function 'indent-relative-maybe)
      (setq indent-region-function nil)
      (set-local-tab-width 2)
      (setq company-auto-complete nil)))
  :config
  (setq fsharp-ac-use-popup t)
  (setq fsharp-indent-offset 2)
  (setq fsharp-continuation-offset 2)
  (setq fsharp-indent-block-under-opening-brace nil)
  (setq inferior-fsharp-program "fsharpi --readline- --shadowcopyreferences+")
  (general-define-key
   :keymaps 'fsharp-mode-map
   "C-m" 'fsharp-maybe-reindent-and-newline-indent
   "C-c C-k" 'fsharp-load-buffer-file
   "C-c C-n" 'fsharp-send-references))

(defun fsharp-maybe-reindent-and-newline-indent ()
  (interactive)
  (let ((ci (current-indentation)))
    (if (< ci (current-column))
        (if (fsharp-outdent-p)
            (reindent-then-newline-and-indent)
          (newline-and-indent))
      (beginning-of-line)
      (insert-char ?\n 1)
      (move-to-column ci))))

(provide 'config-fsharp)
