(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (company-mode 1)
              (set-local-tab-width 2)))
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
  (general-define-key
   :keymaps 'clojure-mode-map
   "C-:" 'eval-expression))

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
              (eldoc-mode 1)
              (company-mode -1)
              (company-quickhelp-mode -1)
              (cider-company-enable-fuzzy-completion)))
  :config
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers nil)
  (setq cider-use-fringe-indicators nil)
  (setq cider-show-error-buffer nil)
  (setq cider-completion-annotations-include-ns t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-lein-parameters "repl :headless :host localhost")
  (setq cider-cljs-lein-repl
        "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
  (setq cider-doc-auto-select-buffer nil)
  (general-define-key
   :keymaps 'cider-mode-map
   "C-c C-n" 'cider-repl-set-ns
   "C-c C-." 'cider-find-var)
  (general-define-key
   :keymaps 'cider-repl-mode-map
   "C-c C-l" 'cider-repl-clear-buffer))

(use-package flycheck-joker
  :ensure t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (flycheck-mode 1)
              (flycheck-pos-tip-enable))))

(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-<return>"))))

(provide 'config-clojure)
