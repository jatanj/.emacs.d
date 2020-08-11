(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c s")
  :config
  (setq lsp-enable-indentation nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-folding nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-completion-no-cache t)
  (setq lsp-diagnostic-clean-after-change nil)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-auto-execute-action nil)
  (setq lsp-modeline-code-actions-segments '(count icon))

  (add-hook 'lsp-mode-hook
            (lambda ()
              ;; Disable company initially until we connect to server
              (company-mode -1)
              (setq-local company-backends '(company-capf))
              (setq-local flycheck-check-syntax-automatically '(save idle-change mode-enabled))))

  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (company-mode )
              (eldoc-mode -1)))

  (defun lsp-evil-jump-to-definition-a (orig &rest args)
    (cond
     ((bound-and-true-p lsp-mode)
      (lsp-find-definition)
      (if (s-starts-with-p "Not found for:" (current-message) t)
          (progn
            (message nil)
            (cider-find-var))))
     ((bound-and-true-p cider-mode) (cider-find-var))
     (t (funcall-interactively orig))))
  (advice-add 'evil-jump-to-definition :around #'lsp-evil-jump-to-definition-a)

  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))
  (dolist (ignored '("[/\\\\]resources$"
                     "[/\\\\]\\.shadow-cljs$"
                     "[/\\\\]\\.lsp$"
                     "[/\\\\]\\.clj-kondo$"))
    (add-to-list 'lsp-file-watch-ignored ignored))

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
         (t (let ((selected (unwind-protect
                                (progn
                                  (framey--enable-helm)
                                  (lsp--select-action actions))
                              (framey--disable-helm))))
              (if (ht-get selected "X-isCustom")
                  (progn
                    (funcall (ht-get selected "X-customHandler")))
                (lsp-execute-code-action selected))))))))

  (general-define-key
   :keymaps 'lsp-mode-map
   "C-<return>" 'lsp-execute-code-action-custom
   "C-c C-s" lsp-command-map
   "C-}" 'lsp-find-references)
  (general-define-key
   :keymaps 'lsp-command-map
   "d" 'lsp-ui-doc-glance
   "C-d" 'lsp-ui-doc-glance))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-ui-sideline-show-symbol nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-doc-enable nil)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (lsp-ui-mode 1)
              (lsp-ui-sideline-mode -1))))

(use-package helm-lsp
  :ensure t
  :after lsp-mode
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  (define-key lsp-mode-map [remap helm-apropos] #'helm-lsp-workspace-symbol))

(provide 'config-lsp)
