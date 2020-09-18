(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c s")
  :config
  (setq lsp-enable-indentation nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-folding nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-completion-no-cache nil)
  (setq lsp-diagnostic-clean-after-change nil)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-auto-execute-action nil)
  (setq lsp-modeline-code-actions-segments '(count icon))

  (defun lsp-enable (&rest args)
    (interactive)
    (when-let* ((file-name (buffer-file-name))
                (_ (and (file-exists-p file-name))))
      (when (and (not lsp--buffer-workspaces)
                 (--any? (eq major-mode (car it)) lsp-language-id-configuration))
        (lsp)
        (company-mode 1))
      (when (eq flycheck-checker 'lsp)
        (lsp-diagnostics--enable)
        (flycheck-buffer))))

  (defun config/lsp-evil-jump-to-definition-a (orig &rest args)
    (let ((do-first-jump (lambda (funcs)
                           (let ((pos (point))
                                 (buffer (current-buffer)))
                             (-any (lambda (func)
                                     (let ((inhibit-message t))
                                       (ignore-errors (call-interactively func)))
                                     (and (not (and (= pos (point))
                                                    (eq buffer (current-buffer))))
                                          func))
                                   funcs))))
          (funcs (-flatten
                  (list (when (bound-and-true-p lsp-mode)
                          '(lsp-find-definition))
                        (when (bound-and-true-p cider-mode)
                          '(config/cider-find-var-at-point))))))
      (let ((result (funcall do-first-jump funcs))
            (inhibit-message t))
        (message "lsp-evil-jump-to-definition-a : %s" result)
        (when (not result)
          (funcall-interactively orig)))))
  (advice-add 'evil-goto-definition :around #'config/lsp-evil-jump-to-definition-a)

  (defun config/lsp-goto-location-a (orig &rest args)
    (let* ((data (car args))
           (uri (ht-get data "uri")))
      (if (s-starts-with? "jar:" uri)
          ;; The JAR URI does not exist on the filesystem so don't attempt to check if it does.
          ;; The original function calls `f-exists?' on the URI which causes it to fail.
          (let ((path (lsp--uri-to-path uri))
                (start (ht-get* data "range" "start")))
            (with-current-buffer (find-file path)
              (goto-char (lsp--position-to-point start))))
        (apply orig args))))
  (advice-add 'lsp-goto-location :around #'config/lsp-goto-location-a)

  (dolist (ignored '("[/\\\\]resources$"
                     "[/\\\\]\\.shadow-cljs$"
                     "[/\\\\]\\.lsp$"
                     "[/\\\\]\\.clj-kondo$"))
    (add-to-list 'lsp-file-watch-ignored ignored))
  (setq lsp-enable-file-watchers nil)

  (setq lsp--custom-code-actions
        (list (ht ("title" "Add missing libspec (CLJR)")
                  ("kind" "quickfix")
                  ("isPreferred" t)
                  ("X-isCustom" t)
                  ("X-customPredicate"
                   '(lambda ()
                      (->> (seq-map #'flycheck-error-message (flycheck-overlay-errors-at (point)))
                           (-any (lambda (s)
                                   (--any (s-starts-with? it s t)
                                          '("Unknown namespace"
                                            "Unresolved symbol"
                                            "Unresolved namespace")))))))
                  ("X-customHandler" #'cljr-add-missing-libspec))))

  (defun lsp-execute-code-action-custom ()
    "Show a LSP code action popup with custom (non-LSP) commands."
    (interactive)
    (let ((actions (lsp-code-actions-at-point)))
      (dolist (a lsp--custom-code-actions)
        (if (and (if-let ((p (ht-get a "X-customPredicate"))) (funcall p) t)
                 (-none? (lambda (x) (eq (ht-get x "title") (ht-get a "title"))) actions))
            (add-to-list 'actions a))
        (cond
         ((seq-empty-p actions)
          (signal 'lsp-no-code-actions nil))
         ((and (eq (seq-length actions) 1) lsp-auto-execute-action)
          (lsp-seq-first actions))
         (t (let ((selected (lsp--select-action actions)))
              (if (ht-get selected "X-isCustom")
                  (progn
                    (funcall (ht-get selected "X-customHandler")))
                (lsp-execute-code-action selected))))))))

  (defun config/lsp-mode-init ()
    ;; Disable company initially until we connect to server
    (when (not lsp--buffer-workspaces)
      (company-mode -1))
    (setq-local company-backends '(company-capf))
    (setq-local company-idle-delay 0)
    (setq-local flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch mode-enabled))
    ; (lsp-ui-sideline-mode -1)
    (eldoc-mode -1))
  (add-hook 'lsp-mode-hook #'config/lsp-mode-init)

  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (add-hook 'switch-buffer-functions #'lsp-enable)))

  (add-hook 'lsp-after-uninitialized-functions
            (lambda ()
              (remove-hook 'switch-buffer-functions #'lsp-enable)))

  (general-define-key
   :keymaps 'lsp-mode-map
   "C-<return>" 'lsp-execute-code-action-custom
   "C-c C-s" lsp-command-map
   "C-c C-r r" 'lsp-rename
   "C-}" 'lsp-find-references)
  (general-define-key
   :keymaps 'lsp-command-map
   ; "d" 'lsp-ui-doc-glance
   ; "C-d" 'lsp-ui-doc-glance
   )

  (general-define-key
   :keymaps '(clojure-mode-map
              clojurescript-mode-map
              web-mode-map
              css-mode-map
              scss-mode-map
              scala-mode-map)
   "C-c C-a C-s" 'lsp)

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*xref" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (side . bottom)
                 (window-height . 0.35)))

  (defun config/lsp-goto-xref ()
    (interactive)
    (let ((window (get-buffer-window (current-buffer))))
      (xref-goto-xref)
      (select-window window)))

  (general-define-key
   :keymaps 'xref--xref-buffer-mode-map
   :states '(normal)
   "RET" 'config/lsp-goto-xref))

(use-package lsp-metals
  :ensure t
  :init
  (defun config/lsp-metals-init ()
    ; (lsp-ui-sideline-mode -1)
    )
  (add-hook 'lsp-metals-after-open-hook #'config/lsp-metals-init))

;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :init
;;   (setq lsp-ui-sideline-show-symbol nil)
;;   (setq lsp-ui-sideline-show-hover nil)
;;   (setq lsp-ui-sideline-show-code-actions nil)
;;   (setq lsp-ui-sideline-show-diagnostics nil)
;;   (setq lsp-ui-peek-enable nil)
;;   (setq lsp-ui-doc-enable nil)
;;   (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package helm-lsp
  :ensure t
  :after (lsp-mode helm-projectile)
  :init
  (setq helm-lsp-treemacs-icons nil)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  (define-key lsp-mode-map [remap helm-apropos] #'helm-lsp-workspace-symbol))

(provide 'config-lsp)
