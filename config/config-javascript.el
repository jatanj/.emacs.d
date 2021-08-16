(use-package rjsx-mode
  :straight t
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode))
  :init
  (setq js-indent-level 2)
  (setq javascript-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js2-mode-assume-strict t)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-idle-timer-delay 0.2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  (defun config/eslint-after-save ()
    "Hack to work around eslint checking breaking periodically."
    (when (config/lsp-enabled-p)
      (let ((inhibit-message t))
        (lsp))))

  (defun config/js2-mode-init ()
    (setq company-minimum-prefix-length 0)
    (when (string= (file-name-extension (buffer-file-name)) "jsx")
      (sgml-electric-tag-pair-mode 1)
      (emmet-mode 1))
    (when (functionp 'lsp)
      (setq-local lsp-enabled-clients '(ts-ls eslint)))
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (add-hook 'config/save-keybind-hook 'config/eslint-after-save nil t))
  (add-hook 'js2-mode-hook #'config/js2-mode-init)

  (defun config/rjsx-newline-and-indent ()
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

  (defun config/webpack-load-aliases ()
    (if-let ((projectile-root (projectile-project-p)))
        (let* ((config-dir (expand-file-name (file-name-as-directory projectile-root)))
               (config-path (concat config-dir ".webpack-alias.json")))
          (when (file-exists-p config-path)
            (->> (json-parse-string (with-temp-buffer
                                      (insert-file-contents config-path)
                                      (buffer-string)))
                 (ht-map (lambda (k v) `(,k . ,(file-truename (concat config-dir v)))))
                 (ht<-alist))))))

  (defun config/lsp-goto-definition-js-resolve-import ()
    (interactive)
    (let ((filename (thing-at-point 'filename 'no-properties)))
      (when (s-starts-with? "@" filename)
        (let* ((aliases (config/webpack-load-aliases))
               (buffer (current-buffer))
               (buffer-contents (buffer-string))
               (bounds (bounds-of-thing-at-point 'filename))
               (remove-trailing-slash  (lambda (p)
                                         (if (s-ends-with? "/" p)
                                             (substring p 0 (- (length p) 1)))))
               (restore-buffer (lambda (p)
                                 (with-current-buffer buffer
                                   (goto-char p)
                                   (let ((b (bounds-of-thing-at-point 'filename)))
                                     (goto-char (car b))
                                     (delete-region (car b) (cdr b))
                                     (insert filename)
                                     (set-buffer-modified-p nil)))))
               (resolve-aliases (lambda (path)
                                  (let ((resolved (->> (s-split "/" path)
                                                       (-map (lambda (p) (or (ht-get aliases p) p))))))
                                    (string-join resolved "/"))))
               (resolved-filename (funcall resolve-aliases filename)))
          (goto-char (car bounds))
          (delete-region (car bounds) (cdr bounds))
          (insert resolved-filename)))))

  (defun config/lsp-goto-definition-js ()
    (interactive)
    (let ((buffer (current-buffer))
          (window (get-buffer-window))
          (temp-buffer (generate-new-buffer "*config-lsp-js-goto-definition*"))
          (saved-point (point))
          (saved-content (buffer-string)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (when (re-search-forward "\".+[^\"]\"" (line-end-position) t)
            (backward-char)
            (config/lsp-goto-definition-js-resolve-import))
          (forward-line 1)))
      (lsp-find-definition)
      (with-current-buffer temp-buffer
        (insert saved-content))
      (with-current-buffer buffer
        (replace-buffer-contents temp-buffer)
        (set-buffer-modified-p nil))
      (kill-buffer temp-buffer)))

  :config
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil)

  (general-define-key
   :keymaps 'rjsx-mode-map
   "C-c C-e" nil
   "C-c C-t" nil
   "C-c C-w" nil
   "<return>" 'config/rjsx-newline-and-indent
   ">" nil))

(provide 'config-javascript)
