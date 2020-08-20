(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (eldoc-mode -1)
              (config/set-local-tab-width 2)))
  :config
  (setq clojure-indent-style 'always-align)
  (define-clojure-indent
    (match 1)
    (are 2)
    (checking 2)
    (async 1))
  (define-clojure-indent
    (go-loop 1))
  (define-clojure-indent
    (this-as 1)
    (specify 1)
    (specify! 1))
  (define-clojure-indent
    (.then 1))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (general-define-key
               :keymaps 'clojure-mode-map
               "C-:" 'eval-expression
               "C-c C-s" 'lsp-mode-map
               "C-c C-q" nil
               "C-c C-\\" 'clojure-align))
            t)

  (defun config/clojure-resolve-jdk-src-paths ()
    (if-let* ((java-home (file-name-as-directory (file-truename (getenv "JAVA_HOME"))))
              (file-directory-p java-home))
        (--filter (file-exists-p it)
                  (list (concat java-home "src/"))))))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :after clojure-mode)

(use-package cider
  :ensure t
  :after clojure-mode
  :init
  (defun config/cider--append-completion-at-point ()
    (add-hook 'completion-at-point-functions #'cider-complete-at-point nil t))
  (defun config/cider-add-to-lsp-completion ()
    "Configure `completion-at-point-functions' to use both Cider and LSP for completions."
    (when (functionp 'lsp)
      (when lsp--buffer-workspaces
        (cider--override-completion-at-point))
      (add-hook 'lsp-after-open-hook #'cider--append-completion-at-point t)))
  (defun config/cider-reset-to-lsp-completion ()
    (when (and (functionp 'lsp)
               lsp--buffer-workspaces)
      (setq-local completion-at-point-functions nil)
      (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t)))
  ;; (add-hook 'cider-connected-hook #'cider-add-to-lsp-completion)
  ;; (add-hook 'cider-disconnected-hook #'cider-reset-to-lsp-completion)

  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (company-mode 1)
              (hscroll-mode 1)))
  (add-hook 'cider-mode-hook
            (lambda ()
              (setq-local safe-local-variable-values
                          '((cider-shadow-cljs-default-options . "app")
                            (cider-default-cljs-repl . "shadow")))
              (eldoc-mode -1)
              (company-quickhelp-mode -1)))

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*cider-doc" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (side . bottom)
                 (window-height . 0.35)))
  :config
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers nil)
  (setq cider-use-fringe-indicators nil)
  (setq cider-show-error-buffer nil)
  (setq cider-completion-annotations-include-ns t)
  (setq cider-switch-to-repl-after-insert-p nil)
  (setq cider-switch-to-repl-on-insert nil)
  (setq cider-repl-history-file (concat (file-name-as-directory user-emacs-directory) "cider-history"))
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-doc-auto-select-buffer t)
  (setq cider-prompt-for-symbol nil)

  (dolist (path (config/clojure-resolve-jdk-src-paths))
    (add-to-list 'cider-jdk-src-paths path))
  (dolist (path cider-jdk-src-paths)
    (when (not (file-directory-p path))
      (delete path cider-jdk-src-paths)))

  (defun config/cider-connect-cljs-shadow ()
    (interactive)
    (cider-connect-cljs '(:host "localhost" :port 8777 :cljs-repl-type shadow)))

  (defun config/cider-connect-clj-lein ()
    (interactive)
    (cider-connect-clj '(:host "localhost" :port 8788 :cljs-repl-type lein)))

  (defun config/cider-find-matching-repl-buffers (&optional buffer)
    (with-current-buffer (or buffer (current-buffer))
      (-filter (lambda (b)
                 (let ((buffer-major-mode (with-current-buffer b
                                            (cons major-mode mode-name))))
                   (and (eq (car buffer-major-mode) 'cider-repl-mode)
                        (or (and (eq major-mode 'clojurescript-mode)
                                 (string-equal "REPL[cljs]" (cdr buffer-major-mode)))
                            (and (eq major-mode 'clojure-mode)
                                 (string-equal "REPL[clj]" (cdr buffer-major-mode)))))))
               (buffer-list))))

  (defun config/cider-load-buffer (&optional buffer callback)
    "Workaround a weird issue where `cider-load-buffer' does not succeed until
    we switch to the REPL buffer at least once."
    (interactive)
    (let ((repl-buffers (config/cider-find-matching-repl-buffers buffer)))
      (dolist (b repl-buffers)
        (let ((current (or buffer (current-buffer))))
          (switch-to-buffer b)
          (switch-to-buffer current)))
      (cider-load-buffer buffer callback)))

  (defun config/cider-find-var-at-point ()
    (interactive)
    (cider-find-var 0 (cider-symbol-at-point) nil))

  (defun config/cider--jump-to-loc-from-info-java-file-a (orig &rest args)
    (when-let* ((info (car args))
                (class (nrepl-dict-get info "class"))
                (_ (not (nrepl-dict-contains info "file"))))
      (nrepl-dict-put info "file" (cider-resolve-java-class class)))
    (apply orig args))
  (advice-add 'cider--jump-to-loc-from-info :around #'config/cider--jump-to-loc-from-info-java-file-a)

  (defun config/cider-find-java-source-file (pattern)
    (when (s-present? pattern)
      (-mapcat
       (lambda (path)
         (let* ((cmd (format "find '%s' | grep '%s'" (expand-file-name path) pattern))
                (results (shell-command-to-string cmd)))
           (-filter 's-present? (s-split "\n" results))))
       cider-jdk-src-paths)))

  (defun config/cider-jump-to-java-file-find-grep ()
    (interactive)
    (let* ((parts (s-split "/" (cider-symbol-at-point)))
           (file (or (cider-resolve-java-class (car parts))
                     (car (config/cider-find-java-source-file (car parts))))))
      (find-file file)))

  (general-define-key
   :keymaps 'cider-mode-map
   "C-c C-q" nil
   "C-c C-v" nil
   "C-c C-j" nil
   "C-c C-f" nil
   "C-c C-k" 'config/cider-load-buffer
   "C-c C-n" 'cider-repl-set-ns
   "C-c C-c" 'cider-eval-commands-map
   "C-c C-v" 'cider-insert-commands-map
   "C-c C-j" 'cider-start-map
   "C-c C-x" nil
   "C-c C-]" 'cider-find-var
   "C-\"" 'cider-apropos)
  (general-define-key
   :keymaps 'cider-eval-commands-map
   "q" 'cider-inspect-last-result
   "C-q" 'cider-inspect-last-result)
  (general-define-key
   :keymaps 'cider-repl-mode-map
   "C-c C-l" 'cider-repl-clear-buffer
   "<return>" 'cider-repl-return)
  (general-define-key
   :keymaps 'clojure-mode-map
   "C-c C-a C-c C-j" 'config/cider-connect-clj-lein
   "C-c C-a C-c C-s" 'config/cider-connect-cljs-shadow))

(use-package helm-cider
  :ensure t
  :after cider
  :init
  (add-hook 'cider-mode-hook #'helm-cider-mode))

(use-package flycheck-clj-kondo
  :ensure t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (require 'flycheck-clj-kondo)
              (flycheck-mode 1))))

(use-package clj-refactor
  :ensure t
  :after clojure-mode
  :init
  (setq cljr-add-ns-to-blank-clj-files t)
  (setq cljr-magic-requires nil)
  (setq cljr-auto-clean-ns nil)
  (setq cljr-auto-sort-ns nil)
  (setq cljr-auto-eval-ns-form nil)
  (setq cljr-warn-on-eval nil)

  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-q"))))

(provide 'config-clojure)
