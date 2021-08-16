(use-package web-mode
  :straight t
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.html?\\'"     . web-mode)
         ;; ("\\.tsx\\'"       . web-mode)
         )
  :init
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq nxml-child-indent 2)
              (config/set-local-tab-width 2)))

  (defun config/web-mode-init ()
    (sgml-electric-tag-pair-mode 1)
    (emmet-mode 1))
  (add-hook 'web-mode-hook #'config/web-mode-init)

  (defun config/web-mode-newline-and-indent ()
    (interactive)
    (newline-and-indent)
    (let ((closing-braces '("}" "\\]" ")")))
      (when (or (save-excursion
                  (beginning-of-line)
                  (-any (lambda (x) (looking-at (format "[\t ]*%s[\t ]*" x)))
                        closing-braces))
                (save-excursion
                  (beginning-of-line)
                  (looking-at "[\t ]*<\[\t ]*")))
        (previous-line)
        (end-of-line)
        (newline-and-indent))))

  :config
  (setq sgml-basic-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-current-element-highlight t)

  (general-define-key
   :keymaps 'web-mode-map
   :states 'insert
   "<return>" 'config/web-mode-newline-and-indent))

(use-package emmet-mode
  :straight t
  :init
  (setq emmet-move-cursor-between-quotes t)

  (defun config/emmet-mode-init ()
    (when (member (file-name-extension (buffer-file-name)) '("jsx" "tsx"))
      (setq-local emmet-expand-jsx-className? t)))
  (add-hook 'emmet-mode-hook #'config/emmet-mode-init)

  (defun config/emmet-expand-line ()
    (interactive)
    (let ((expand (lambda ()
                    (backward-word)
                    (forward-word)
                    (emmet-expand-line nil))))
      (when (not (looking-back "[ \t\n]"))
        (funcall expand))))

  (defun config/emmet-electric-gt ()
    (interactive)
    (let* ((backslash (looking-back "/[ \t]*"))
           (opening-tag (save-excursion (re-search-backward "<" (line-beginning-position) t))))
      (if (and opening-tag (not backslash))
          (progn
            (when (looking-at ">")
              (delete-forward-char 1))
            (goto-char opening-tag)
            (delete-forward-char 1)
            (re-search-forward "[A-Z0-9a-z#.]+" (line-end-position) t)
            (backward-char)
            (config/emmet-expand-line))
        (if (looking-at ">")
            (forward-char 1)
          (insert-char ?>)))))

  :config
  (general-define-key
   :keymaps 'emmet-mode-keymap
   "C-<return>" nil
   "C-c C-c" nil
   "C-j" 'config/emmet-expand-line))

(use-package scss-mode
  :straight t
  :mode ("\\.scss\\'" . scss-mode)
  :init
  (defun config/flycheck-scss-set-stylelintrc-file ()
    (let* ((stylelintrc ".stylelintrc.json")
           (candidates (list (concat (file-name-as-directory (or (projectile-project-p) "")) stylelintrc)
                             (expand-file-name (concat "~/" stylelintrc)))))
      (setq-local flycheck-stylelintrc (--first (file-exists-p it) candidates))))

  (add-hook 'scss-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'scss-stylelint)
              (config/flycheck-scss-set-stylelintrc-file)
              (flycheck-mode 1)
              (config/set-local-tab-width 2)))
  :config
  (setq css-indent-offset 2)
  (general-define-key
   :keymaps 'scss-mode-map
   "C-c C-c" nil))

(provide 'config-web)
