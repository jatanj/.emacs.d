(require 'company)

(setq company-frontends
      '(company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(setq company-require-match nil)

(add-hook 'after-init-hook 'global-company-mode)

;; Disable completion when some key is pressed
(defun company-abort-and-insert-char (char)
  (interactive)
  (company-abort)
  (insert char)
  (let ((inhibit-message t)) (company-complete)))

(general-define-key
 :keymaps 'company-active-map
 "<f1>" nil
 "<tab>" 'company-complete
 "<return>" (lookup-key (current-global-map) (kbd "RET"))
 "." (lambda () (interactive) (company-abort-and-insert-char ".")))

(defun my-lookup-key (key)
  "Search for KEY in all known keymaps."
  (mapatoms (lambda (ob) (when (and (boundp ob) (keymapp (symbol-value ob)))
                      (when (functionp (lookup-key (symbol-value ob) key))
                        (message "%S" ob))))
            obarray))

(provide 'config-company)
