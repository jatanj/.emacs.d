(use-package evil
  :straight t
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
                  flycheck-error-list-mode))
    (evil-set-initial-state mode 'emacs))

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

  (setq evil-jump-count 10)
  (defun evil-previous-line-jump ()
    (interactive)
    (evil-previous-line evil-jump-count))
  (defun evil-next-line-jump ()
    (interactive)
    (evil-next-line evil-jump-count))
  (defun evil-forward-char-jump ()
    (interactive)
    (evil-forward-char evil-jump-count))
  (defun evil-backward-char-jump ()
    (interactive)
    (evil-backward-char evil-jump-count))
  (evil-declare-motion 'evil-next-line-jump)
  (evil-declare-motion 'evil-previous-line-jump)
  (evil-declare-motion 'evil-forward-char-jump)
  (evil-declare-motion 'evil-backward-char-jump)

  (defun config/evil-goto-definition ()
    "We create our own goto definition function here so LSP mode can advice it without
     touching `evil-goto-definition'."
    (interactive)
    (evil-goto-definition))

  (general-define-key
   :states 'insert
   "C-k" ctl-x-map
   "C-g" 'evil-force-normal-state)

  (general-define-key
   :states 'visual
   ">" 'evil-shift-right-visual
   "<" 'evil-shift-left-visual)

  (general-define-key
   :states '(normal insert visual)
   "S-<up>" 'evil-previous-line-jump
   "S-<down>" 'evil-next-line-jump
   "S-<left>" 'evil-backward-char-jump
   "S-<right>" 'evil-forward-char-jump)

  (general-define-key
   :states '(normal visual)
   "SPC" (general-simulate-key "C-k")
   "J" 'tabbar-backward-tab
   "K" 'tabbar-forward-tab)

  (general-define-key
   :states '(normal insert visual)
   "C-s" 'save-buffer
   "C-f" 'isearch-forward-regexp
   "C-S-f" 'isearch-backward-regexp
   "C-h" 'query-replace-regexp
   "C-S-h" 'anzu-query-replace-at-cursor-thing
   "C-b" 'do-nothing)

  (general-define-key
   :states '(normal insert visual)
   "C-]" 'config/evil-goto-definition
   "C-q" 'evil-scroll-line-up)

  (general-define-key
   :states '(normal insert visual emacs)
   "<home>" 'back-to-indentation))

;; Improve shift to keep selection
;; http://superuser.com/questions/684540/#answer-789156
(defun evil-shift-visual (shift)
  (funcall shift (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))
(defun evil-shift-left-visual () (interactive) (evil-shift-visual 'evil-shift-left))
(defun evil-shift-right-visual () (interactive) (evil-shift-visual 'evil-shift-right))

(use-package evil-anzu
  :straight t)

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :straight t
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-matchit
  :straight t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-args
  :straight t
  :config
  ;; Bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  (add-to-list 'evil-args-delimiters " "))

(use-package evil-exchange
  :straight t
  :config
  (evil-exchange-install))

(use-package evil-lion
  :straight t
  :config
  (evil-lion-mode))

(provide 'config-evil)
