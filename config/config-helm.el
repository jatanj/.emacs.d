(use-package helm
  :straight t
  :config
  (defalias 'browse-url-mosaic-program #'browse-url-chromium)
  (require 'helm-config)

  (helm-mode 1)
  (helm-autoresize-mode 1)

  (setq helm-autoresize-max-height 35)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-session-fuzzy-match t)
  (setq helm-etags-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-style 'helm-fuzzy)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-mode-handle-completion-in-region t)
  (setq helm-display-header-line nil)
  (setq helm-split-window-in-side-p t)
  (setq helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (side . bottom)
                 (window-height . 0.35)))

  (setq helm-boring-buffer-regexp-list
        '("\\` "
          "\\*helm"
          "\\*helm-mode"
          "\\*Echo Area"
          "\\*tramp"
          "\\*Minibuf"
          "\\*Ibuffer\\*"
          "\\*tide-server\\*"
          "\\*fsharp-complete\\*"))
  (defun helm-custom-filter-buffers-a (buffer-list)
    (delq nil (mapcar
               (lambda (buffer)
                 (cond
                  ((eq (with-current-buffer buffer major-mode) 'dired-mode) nil)
                  (t buffer)))
               buffer-list)))
  (advice-add 'helm-skip-boring-buffers :filter-return 'helm-custom-filter-buffers-a)

  (defun config/helm-skip-dots (old-func &rest args)
    "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
    (apply old-func args)
    (let ((sel (helm-get-selection)))
      (if (and (stringp sel) (string-match "/\\.$" sel))
          (helm-next-line 2)))
    (let ((sel (helm-get-selection))) ; if we reached .. move back
      (if (and (stringp sel) (string-match "/\\.\\.$" sel))
          (helm-previous-line 1))))
  (advice-add #'helm-preselect :around #'config/helm-skip-dots)
  (advice-add #'helm-ff-move-to-first-real-candidate :around #'config/helm-skip-dots)

  (global-set-key (kbd "C-S-p") 'helm-M-x)
  (global-set-key (kbd "C-p") 'helm-buffers-list)
  (general-define-key
   :prefix leader-key
   "f" 'helm-find))

(use-package helm-projectile
  :straight t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package helm-ag
  :straight t
  :config
  (setq helm-ag-fuzzy-match t)
  (setq helm-ag-base-command "rg --smart-case --no-heading")
  (general-define-key
   :keymaps 'projectile-command-map
   "s s" 'helm-do-ag-project-root
   "s f" 'helm-do-ag-this-file
   "s b" 'helm-do-ag-buffers))

(use-package helm-xref
  :straight t)

(provide 'config-helm)
