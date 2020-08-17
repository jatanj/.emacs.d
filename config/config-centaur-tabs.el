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
  (centaur-tabs-enable-buffer-alphabetical-reordering)
  (setq centaur-tabs-adjust-buffer-order t)

  ;; When in daemon mode, this needs to be run after the frame is created to ensure
  ;; (display-graphic-p) returns t.
  (if (daemonp)
      (add-hook 'configure-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (set-face-attribute 'header-line nil :background (car (get 'custom-theme-color-bg6 'saved-value)))
                    (centaur-tabs-headline-match)
                    (centaur-tabs-mode 1))))
    (centaur-tabs-mode 1))

  ;; Hack -- the tabs don't seem to render correctly until we switch tabs at least once.
  (add-hook 'configure-frame-functions
            (lambda (frame)
              (when (bound-and-true-p centaur-tabs-mode)
                (centaur-tabs-forward)
                (centaur-tabs-backward))))

  (defvar centaur-tabs--projectile-project-cache (ht-create)
    "Cached projectile project roots to improve performance of `centaur-tabs-buffer-groups'")

  (defun centaur-tabs-cached-projectile-root (dir)
    "Find the projectile project root for DIR."
    (or (ht-get centaur-tabs--projectile-project-cache dir)
        (when-let* ((projectile-root (projectile-project-p dir)))
          (ht-set centaur-tabs--projectile-project-cache dir projectile-root)
          projectile-root)))

  (defun centaur-tabs-clear-projectile-cache ()
    "Clear the projectile project cache."
    (interactive)
    (setq centaur-tabs--projectile-project-cache (ht-create)))

  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
    (let ((file (buffer-file-name)))
      (list
       (cond
        ((memq major-mode '(cider-repl-mode))
         "Repl")
        ((or (memq major-mode '(magit-process-mode
                                magit-status-mode
                                magit-diff-mode
                                magit-log-mode
                                magit-blob-mode
                                magit-blame-mode
                                magit-revision-mode))
             (and (memq major-mode '(magit-file-mode-hook))
                  (not buffer-file-name)))
         "Magit")
        ((string-equal "*" (substring (buffer-name) 0 1))
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
        ((and (stringp file)
              (string-match-p ".jar:" file))
         "Jars")
        (t (or (if-let* ((projectile-root (centaur-tabs-cached-projectile-root file)))
                   projectile-root
                 (when-let* ((group-name (centaur-tabs-get-group-name (current-buffer)))
                             (stringp group-name))
                   group-name))
               "Other"))))))

  (defalias 'tabbar-mode 'centaur-tabs-mode)
  (defalias 'tabbar-backward-tab 'centaur-tabs-backward)
  (defalias 'tabbar-forward-tab 'centaur-tabs-forward)

  (defun centaur-tabs-local-disable ()
    "Disable centaur-tabs-mode if it's currently enabled."
    (interactive)
    (when (bound-and-true-p centaur-tabs-mode)
      (ignore-errors (centaur-tabs-local-mode))))
  (defalias 'tabbar-local-disable 'centaur-tabs-local-disable)

  (defun centaur-tabs-blend-header-line (&optional text)
    (face-remap-add-relative 'header-line `(:box (:line-width 7 :color ,(car (get 'custom-theme-color-bg6 'saved-value)))))
    (setq header-line-format
          (concat (propertize " " 'display '((space :align-to 0)))
                  (or (propertize text 'face `(:foreground ,(car (get 'custom-theme-color-delim 'saved-value)) :family "Fira Code Light" :height 110)) " "))))
  (defalias 'tabbar-blend-header-line 'centaur-tabs-blend-header-line)

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
