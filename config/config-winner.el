(use-package winner
  :config
  (winner-mode)
  (general-define-key
   :keymaps 'winner-mode-map
   "C-<left>" nil
   "C-<right>" nil)
  (general-define-key
   :keymaps 'ctl-x-map
   "1" 'delete-other-windows-winner
   "8" 'winner-redo
   "9" 'winner-undo))

  (defun delete-other-windows-winner (&optional window)
    "Delete other windows if WINDOW is not a side window. Otherwise, just switch
to the buffer of WINDOW in another non-side window."
    (interactive)
    (let ((window (or window (selected-window))))
      (if (window-parameter window 'window-side)
          (let ((buffer (window-buffer window))
                (non-side-window (car (let (windows)
                                        (walk-window-tree
                                         (lambda (win)
                                           (unless (window-parameter win 'window-side)
                                             (push win windows))))
                                        windows))))
            (when (and (window-parameter window 'window-side)
                       non-side-window
                       (buffer-live-p buffer))
              (delete-window window)
              (select-window non-side-window)
              (with-current-buffer buffer
                (tabbar-blend-header-line "Popup"))
              (switch-to-buffer buffer)))
      (delete-other-windows window))))

(provide 'config-winner)
