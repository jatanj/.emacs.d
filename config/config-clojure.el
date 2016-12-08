(require 'clojure-mode)
(require 'cider)

(setq clojure-indent-style :always-align)

(add-hook 'clojure-mode-hook
  (lambda ()
    (company-mode 1)
    (set-local-tab-width 2)))

(general-define-key
 :keymaps 'clojure-mode-map
 "C-:" 'eval-expression)

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

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)

(setq cider-cljs-lein-repl
  "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")

(general-define-key
 :keymaps 'cider-mode-map
 "C-c C-n" 'cider-repl-set-ns
 "C-c C-." 'cider-find-var)
(general-define-key
 :keymaps 'cider-repl-mode-map
 "C-c C-l" 'cider-repl-clear-buffer)

(provide 'config-clojure)
