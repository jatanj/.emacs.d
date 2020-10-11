(use-package super-save
  :straight t
  :init
  (setq super-save-auto-save-when-idle nil)
  (setq super-save-remote-files nil)

  (defun config/super-save-command-a (orig-fun &rest args)
    (let ((inhibit-message t))
      (apply orig-fun args)))
  (advice-add 'super-save-command :around #'config/super-save-command-a)

  :config
  (super-save-mode 1))

(provide 'config-super-save)
