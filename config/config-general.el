(setq leader-key "C-l")
(general-define-key :prefix leader-key)

(defun do-nothing ()
  (interactive)
  nil)

(provide 'config-general)
