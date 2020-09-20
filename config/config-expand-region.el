(use-package expand-region
  :straight t
  :config
  (add-hook 'org-mode-hook
    (lambda ()
      (make-variable-buffer-local 'er/try-expand-list)
      (setq er/try-expand-list (append
                                er/try-expand-list
                                '(er/mark-org-table-field
                                  er/mark-org-table-row
                                  er/mark-org-table-full)))))
  (general-define-key
   :states '(normal visual)
   "=" 'er/expand-region
   "-" 'er/contract-region))

(defun er/org-forward-table-column (n)
  (let ((start (point))
        (forward-col
         (lambda (n)
           (let ((col (current-column)))
             (forward-line n)
             (move-to-column col)))))
    (unless (save-excursion
              (forward-line n)
              (or (eobp)
                  (bobp)))
      (funcall forward-col n)
      (let ((in-table (org-at-table-p)))
        (unless in-table
          (goto-char start))
        in-table))))

(defun er/mark-org-table-field ()
  "Marks an org table field."
  (interactive)
  (when (org-at-table-p)
    (let ((delim "|"))
      (when (re-search-backward delim (line-beginning-position) t)
        (forward-char)
        (set-mark (point))
        (when (re-search-forward delim (line-end-position) t)
          (backward-char)
          (exchange-point-and-mark))))))

(defun er/mark-org-table-row ()
  "Marks an org table row."
  (interactive)
  (when (org-at-table-p)
    (let ((delim "|"))
      (unless (= (org-table-current-column) 1)
        (org-table-beginning-of-field -1))
      (while (re-search-backward delim (line-beginning-position) t))
      (when (looking-at delim)
        (forward-char)
        (set-mark (point))
        (while (re-search-forward delim (line-end-position) t))
        (backward-char)
        (exchange-point-and-mark)))))

;; (defun er/mark-org-table-column ()
;;   (when (org-at-table-p)
;;     (let ((delim "|")
;;           start end)
;;       (while (er/org-forward-table-column -1))
;;       (when (re-search-backward delim (line-beginning-position) t)
;;         (forward-char)
;;         (setq start (point))
;;         (when (re-search-forward delim (line-end-position) t)
;;           (backward-char)
;;           (while (er/org-forward-table-column 1))
;;           (setq end (point))
;;           (when (and start end)
;;             (goto-char start)
;;             (set-mark (point))
;;             (goto-char end)
;;             (exchange-point-and-mark)))))))

(defun er/mark-org-table-full ()
  "Marks an org table."
  (when (org-at-table-p)
    (let ((delim "|"))
      (org-up-element)
      (set-mark (point))
      (while (er/org-forward-table-column 1))
      (while (re-search-forward delim (line-end-position) t))
      (exchange-point-and-mark))))

(provide 'config-expand-region)
