(use-package clojure-mode
  :straight t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :init
  (defun config/clojure-mode-init ()
    (eldoc-mode -1)
    (highlight-indent-guides-mode -1)
    (config/set-local-tab-width 2)
    (when (functionp 'lsp)
      (setq-local lsp-completion-enable nil))
    (general-define-key
     :keymaps 'clojure-mode-map
     "C-:" 'eval-expression
     "C-c C-w" clojure-refactor-map
     "C-c C-r" nil
     "C-c C-s" nil
     "C-c C-q" nil
     "C-c C-\\" 'clojure-align))
  (add-hook 'clojure-mode-hook #'config/clojure-mode-init)
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

  (defun config/clojure-resolve-jdk-src-paths ()
    (if-let* ((java-home (file-name-as-directory (file-truename (getenv "JAVA_HOME"))))
              (file-directory-p java-home))
        (--filter (file-exists-p it)
                  (list (concat java-home "src/"))))))

(use-package clojure-mode-extra-font-locking
  :straight t
  :after clojure-mode)

(use-package cider
  :straight t
  :after (evil clojure-mode)
  :init
  (defun config/cider-set-completion-at-point ()
    "Configure `completion-at-point-functions' to use Cider first instead of LSP."
    (setq-local completion-at-point-functions nil)
    (add-hook 'completion-at-point-functions #'cider-complete-at-point t t)
    (when (and (functionp 'lsp) lsp--buffer-workspaces)
      (add-hook 'completion-at-point-functions #'lsp-completion-at-point t t)))
  (defun config/cider-reset-lsp-completion ()
    (when (and (functionp 'lsp)
               lsp--buffer-workspaces)
      (setq-local completion-at-point-functions nil)
      (add-hook 'completion-at-point-functions #'lsp-completion-at-point nil t)))
  ;; (add-hook 'cider-file-loaded-hook #'config/cider-set-completion-at-point)

  (defun config/cider-mode-init ()
    (setq-local safe-local-variable-values
                '((cider-shadow-cljs-default-options . "app")
                  (cider-default-cljs-repl . "shadow")))
    (eldoc-mode -1)
    (company-mode 1)
    (company-quickhelp-mode -1))
  (add-hook 'cider-mode-hook #'config/cider-mode-init)

  (defun config/cider-repl-mode-init ()
    (company-mode 1)
    (hscroll-mode 1))
  (add-hook 'cider-repl-mode-hook #'config/cider-repl-mode-init)

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
  (setq cider-infer-remote-nrepl-ports t)
  (setq cider-eldoc-display-for-symbol-at-point nil)

  (evil-set-initial-state 'cider-repl-mode 'emacs)

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

  (defun config/cider-connect-bb ()
    (interactive)
    (cider-connect-clj '(:host "localhost" :port 1667 :cljs-repl-type babashka)))

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

  (defun config/cider-define-keybindings ()
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
     :keymaps cider-start-map
     "C-k" 'cider-ns-refresh)
    (general-define-key
     :keymaps 'cider-repl-mode-map
     "C-c C-l" 'cider-repl-clear-buffer
     "<return>" 'cider-repl-return
     "<tab>" 'company-complete-selection-or-indent)
    (general-define-key
     :keymaps 'clojure-mode-map
     "C-c C-a C-c C-j" 'config/cider-connect-clj-lein
     "C-c C-a C-c C-s" 'config/cider-connect-cljs-shadow
     "C-c C-a C-c C-b" 'config/cider-connect-bb))
  (config/cider-define-keybindings))

(use-package helm-cider
  :straight t
  :after (helm cider)
  :init
  ;; Redefine keybindings afterwards to overwrite whatever helm-cider defines.
  (add-hook 'cider-mode-hook #'helm-cider-mode)
  (add-hook 'helm-cider-mode-hook #'config/cider-define-keybindings))

(use-package flycheck-clj-kondo
  :straight t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (require 'flycheck-clj-kondo)
              (flycheck-mode 1))))

(use-package clj-refactor
  :straight t
  :after clojure-mode
  :init
  (setq cljr-add-ns-to-blank-clj-files t)
  (setq cljr-magic-requires nil)
  (setq cljr-auto-clean-ns nil)
  (setq cljr-auto-sort-ns nil)
  (setq cljr-auto-eval-ns-form nil)
  (setq cljr-warn-on-eval nil)
  (setq cljr-suppress-middleware-warnings t)

  (defun config/clj-refactor-init ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-q"))
  (add-hook 'clojure-mode-hook #'config/clj-refactor-init)
  (general-define-key
   :keymaps 'clj-refactor-map
   "/" nil))

(use-package html-to-hiccup
  :straight t
  :init
  (defun custom/html-to-hiccup-convert-region (start end)
    (interactive "r")
    (html-to-hiccup-convert-region start end t))
  :config
  (general-define-key
   :keymaps '(clojure-mode-map
              clojurescript-mode-map)
   "C-c C-a C-h" 'custom/html-to-hiccup-convert-region))

(provide 'config-clojure)
