(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-toggle-key "<f5>")
  :config
  (evil-mode 1)
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-disable-insert-state-bindings t)
  (setq-default evil-shift-width 2)
  (dolist (mode '(Buffer-menu-mode
                  neotree-mode
                  flycheck-error-list-mode
                  cider-repl-mode))
    (evil-set-initial-state mode 'emacs))

  ;; Unbind some keys.
  ;; No idea why this doesn't work with general.
  (defun evil-unbind-key (pair)
    (let ((unbind (lambda (state key)
                    (define-key
                      (symbol-value (intern (format "evil-%s-state-map" (symbol-name state))))
                      (kbd key)
                      nil))))
      (pcase pair
        (`(all ,key) (dolist (state '(normal insert visual motion))
                       (funcall unbind state key)))
        (`(,state ,key) (funcall unbind state key)))))
  (mapcar #'evil-unbind-key '((normal "q")
                              (all "C-p")
                              (all "C-S-p")))

  (general-define-key
   :states 'insert
   "C-k" ctl-x-map
   "C-g" 'evil-force-normal-state
   "<tab>" 'company-complete-selection-or-indent
   "S-<up>" (lambda () (interactive) (evil-previous-line 10))
   "S-<down>" (lambda () (interactive) (evil-next-line 10))
   "S-<left>" (lambda () (interactive) (evil-backward-char 10))
   "S-<right>" (lambda () (interactive) (evil-forward-char 10)))

  (general-define-key
   :states 'visual
   ">" 'evil-shift-right-visual
   "<" 'evil-shift-left-visual)

  (general-define-key
   :states '(normal visual)
   "SPC" (general-simulate-key "C-k"))

  (general-define-key
   :states '(normal insert visual)
   "C-s" 'save-buffer
   "C-f" 'isearch-forward-regexp
   "C-S-f" 'isearch-backward-regexp
   "C-h" 'query-replace-regexp
   "C-S-h" 'anzu-query-replace-at-cursor-thing
   "C-b" 'do-nothing)

   (general-define-key
   :states '(normal visual motion)
   "J" 'tabbar-backward-tab
   "K" 'tabbar-forward-tab
   "S-<up>" (lambda () (interactive) (evil-previous-line 10))
   "S-<down>" (lambda () (interactive) (evil-next-line 10))
   "S-<left>" (lambda () (interactive) (evil-backward-char 10))
   "S-<right>" (lambda () (interactive) (evil-forward-char 10)))

  (general-define-key
   :states '(normal insert visual motion)
   "C-]" 'evil-jump-to-definition
   "C-q" 'evil-scroll-line-up)

  (general-define-key
   :states '(normal insert visual emacs motion)
   "<home>" 'back-to-indentation))

;; Improve shift to keep selection
;; http://superuser.com/questions/684540/#answer-789156
(defun evil-shift-visual (shift)
  (funcall shift (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))
(defun evil-shift-left-visual () (interactive) (evil-shift-visual 'evil-shift-left))
(defun evil-shift-right-visual () (interactive) (evil-shift-visual 'evil-shift-right))

(setq local-jump-to-definition nil)
(defun evil-jump-to-definition ()
  (interactive)
  (if (and (boundp 'local-jump-to-definition)
           local-jump-to-definition)
      (funcall-interactively local-jump-to-definition)
    (call-interactively 'dumb-jump-go)))

(use-package evil-anzu
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-args
  :ensure t
  :config
  ;; Bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  (add-to-list 'evil-args-delimiters " "))

(use-package evil-exchange
  :ensure t
  :config
  (evil-exchange-install))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(provide 'config-evil)
