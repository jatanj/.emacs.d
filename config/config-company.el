(require 'company)

(setq company-frontends
      '(company-pseudo-tooltip-frontend))
        ;; company-echo-metadata-frontend))

(setq company-idle-delay 0.1)
(add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
;; (setq company-require-match nil)
;; (add-hook 'after-init-hook 'global-company-mode)

;; Abort completion when some key is pressed
(defun company-abort-and-insert-char (char)
  (interactive)
  (company-abort)
  (insert char)
  (let ((inhibit-message t)) (company-complete)))

(general-define-key
 :keymaps 'company-active-map
 "<f1>" nil
 "<tab>" 'company-complete-selection
 "<return>" (lookup-key (current-global-map) (kbd "RET"))
 "." (lambda () (interactive) (company-abort-and-insert-char ".")))

(provide 'config-company)
