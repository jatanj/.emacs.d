(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; Disable company initially until we connect to server
              (company-mode -1)
              (eldoc-mode -1)
              (set-local-tab-width 2)
              (rainbow-delimiters-mode-enable)))
  :config
  (setq clojure-indent-style :always-align)
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
               "C-c C-q" nil))
            t))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :after clojure-mode)

(use-package cider
  :ensure t
  :after clojure-mode
  :init
  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (company-mode 1)
              (cider-company-enable-fuzzy-completion)))
  (add-hook 'cider-mode-hook
            (lambda ()
              (eldoc-mode -1)
              (company-quickhelp-mode -1)
              (setq-local completion-at-point-functions nil)
              (setq-local help-window-select t)
              (if (bound-and-true-p lsp-mode)
                  (add-hook 'completion-at-point-functions #'lsp-completion-at-point t))))
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
  (setq cider-lein-parameters "repl :headless :host localhost")
  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
  (setq cider-doc-auto-select-buffer t)
  (setq safe-local-variable-values
        '((cider-shadow-cljs-default-options . "app")
          (cider-default-cljs-repl . "shadow")))
  (setq cider-prompt-for-symbol nil)

  (general-define-key
   :keymaps 'cider-mode-map
   "C-c C-q" nil
   "C-c C-v" nil
   "C-c C-j" nil
   "C-c C-f" nil
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
   "<return>" 'cider-repl-return))

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
              ;; (setq-local flycheck-disabled-checkers '(lsp))
              ;; (setq-local lsp-diagnostic-package :none)
              (flycheck-mode 1))))

(use-package clj-refactor
  :ensure t
  :after clojure-mode
  :init
  (setq cljr-add-ns-to-blank-clj-files nil)
  (setq cljr-magic-requires nil)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-q"))))

(provide 'config-clojure)
