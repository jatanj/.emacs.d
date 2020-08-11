(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-icon-v-adjust 0)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
	(setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-hide-hash (make-hash-table :test 'equal))
  (setq centaur-tabs-hide-tabs-hooks '(reb-mode-hook completion-list-mode-hook))
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-adjust-buffer-order t)
  (setq centaur-tabs-adjust-buffer-order 'left)

  ;; When in daemon mode, this needs to be run after the frame is created to ensure
  ;; (display-graphic-p) returns t.
  (if (daemonp)
      (add-hook 'configure-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (set-face-attribute 'header-line nil :background (car (get 'custom-theme-face-bg6 'saved-value)))
                    (centaur-tabs-headline-match)
                    (centaur-tabs-mode 1))))
    (centaur-tabs-mode 1))

  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       ;; (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       ;; (string-prefix-p "*Help" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((memq major-mode '(cider-repl-mode))
       "Repl")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              magit-revision-mode)))
       "Emacs")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      ((string-match-p ".jar:" (buffer-file-name))
       "Jars")
      (t (let ((projectile-root (projectile-project-root)))
           (or projectile-root (centaur-tabs-get-group-name (current-buffer))))))))

  (defalias 'tabbar-mode 'centaur-tabs-mode)
  (defalias 'tabbar-backward-tab 'centaur-tabs-backward)
  (defalias 'tabbar-forward-tab 'centaur-tabs-forward)

  (defun tabbar-local-disable ()
    "Disable tabbar-mode if it's currently enabled."
    (interactive)
    (when (bound-and-true-p tabbar-mode)
      (ignore-errors (tabbar-mode))))

  (defun tabbar-blend-header-line (&optional text)
    (face-remap-add-relative 'header-line `(:box (:line-width 7 :color ,(car (get 'custom-theme-face-bg6 'saved-value)))))
    (setq header-line-format
          (concat (propertize " " 'display '((space :align-to 0)))
                  (or (propertize text 'face `(:foreground ,(car (get 'custom-theme-face-delim 'saved-value)) :family "Fira Code Light" :height 130)) " "))))

  (general-define-key
   :keymaps 'centaur-tabs-mode-map
   "C-<prior>" 'centaur-tabs-backward
   "C-<next>" 'centaur-tabs-forward)

  ;; Disable default tabbar keybindings
  (general-define-key
   :keymaps 'centaur-tabs-mode-map
   :prefix "C-c"
   "C-<up>" nil
   "C-<down>" nil
   "C-<left>" nil
   "C-<right>" nil
   "C-<prior>" nil
   "C-<next>" nil
   "C-<home>" nil
   "C-<f10>" nil))

(provide 'config-centaur-tabs)
