(use-package flycheck
  :ensure t
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  ;; https://github.com/amperser/proselint
  (when (executable-find "proselint")
    (flycheck-define-checker proselint
      "A linter for prose."
      :command ("proselint" source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": "
                (id (one-or-more (not (any " "))))
                (message (one-or-more not-newline)
                         (zero-or-more "\n" (any " ") (one-or-more not-newline)))
                line-end))
      :modes (text-mode markdown-mode gfm-mode))
    (add-to-list 'flycheck-checkers 'proselint))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (preserve-size . t)
                 (window-height   . 0.2)))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck error messages*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (preserve-size . t)
                 (window-height   . 0.2)))
  (general-define-key
   :keymaps 'flycheck-mode-map
   "C-c ! f" 'flycheck-toggle-fix
   "C-c <C-up>" 'flycheck-previous-error
   "C-c <C-down>" 'flycheck-next-error))

(defun turn-on-proselint ()
  (interactive)
  (when (member 'proselint (flycheck-defined-checkers))
    (flycheck-mode 1)
    (flycheck-select-checker 'proselint)))

(defun flycheck-toggle-fix ()
  (interactive)
  (flycheck-mode 'toggle)
  (flycheck-mode 'toggle))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (setq flycheck-pos-tip-timeout most-positive-fixnum)
  (defun flycheck-pos-tip-toggle ()
    (interactive)
    (if (bound-and-true-p flycheck-pos-tip-mode)
        (progn
          (flycheck-pos-tip-mode -1)
          (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))
      (progn
        (flycheck-pos-tip-mode 1)
        (setq flycheck-display-errors-function
          (lambda (errors)
            (interactive)
            (unless (company-tooltip-visible-p)
              (flycheck-pos-tip-error-messages errors))
            (flycheck-display-error-messages-unless-error-list errors))))))
  (general-define-key
   :keymaps 'flycheck-mode-map
   "C-c ! t" 'flycheck-pos-tip-toggle))

(provide 'config-flycheck)
