(setq leader-key "C-l")
(global-set-key (kbd leader-key) nil)

(defun do-nothing ()
  (interactive)
  nil)

(provide 'config-general)
