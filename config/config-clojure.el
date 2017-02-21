(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :config
  (setq clojure-indent-style :always-align)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (company-mode 1)
              (set-local-tab-width 2)))
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
  :config
  (setq nrepl-hide-special-buffers t)
  (setq cider-use-fringe-indicators nil)
  (setq cider-show-error-buffer nil)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-cljs-lein-repl
        "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (general-define-key
   :keymaps 'cider-mode-map
   "C-c C-n" 'cider-repl-set-ns
   "C-c C-." 'cider-find-var)
  (general-define-key
   :keymaps 'cider-repl-mode-map
   "C-c C-l" 'cider-repl-clear-buffer))

(provide 'config-clojure)
