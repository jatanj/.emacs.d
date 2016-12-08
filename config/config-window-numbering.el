(defun window-numbering-install-mode-line (&optional position)
  "Do nothing")

(window-numbering-mode 1)

(dolist (n (number-sequence 1 9))
  (global-set-key (kbd (format "C-%s" n)) (intern (format "select-window-%s" n))))

(provide 'config-window-numbering)
