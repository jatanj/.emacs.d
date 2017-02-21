(use-package flyspell
  :after ispell
  :config
  (when (executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US"))

  (defun flyspell-save-word-to-dictionary ()
    (interactive)
    (let ((word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil (car word) (point) (cadr word) (caddr word) (point)))))

  (general-define-key
   :keymaps 'flyspell-mode-map
   "C-c $" nil
   "C-c $ c" 'flyspell-correct-word-before-point
   "C-c $ s" 'flyspell-save-word-to-dictionary))

(provide 'config-flyspell)
