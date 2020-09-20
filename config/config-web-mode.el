(use-package web-mode
  :straight t
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.html?\\'"     . web-mode)
         ("\\.tsx\\'"       . web-mode))
  :init
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq nxml-child-indent 2)
              (config/set-local-tab-width 2)))
  :config
  (setq sgml-basic-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-current-element-highlight t))

(use-package scss-mode
  :straight t
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

(provide 'config-web-mode)
