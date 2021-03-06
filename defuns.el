(require 'dash)
(require 's)

(defun comment-line-or-region ()
  "Comment either the current region if it is active or the current line."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-line 1)))

(defun indent-line-or-region ()
  "Indent either the current region if it is active or the current line."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (progn
      (funcall indent-line-function)
      (evil-next-line))))

(defun open-terminal-here ()
  "Opens a terminal window in the current buffer's directory."
  (interactive)
  (let* ((terminal (or (and local-terminal
                            (let ((file (executable-find local-terminal)))
                              (when file (list local-terminal file))))
                       (car (->> local-terminals
                                 (-map (lambda (x) (list x (executable-find x))))
                                 (-filter (lambda (x) (nth 1 x))))))))
    (if terminal
        (let* ((buffer-directory (file-name-directory (file-truename (buffer-file-name))))
               (args (pcase (car terminal)
                       ("xfce4-terminal" `("--default-working-directory" ,buffer-directory))
                       (_ nil))))
          (apply 'call-process (nth 1 terminal) nil 0 nil args))
      (message "Unable to find terminal executable"))))

(defun shell-command-replace-region (command)
  "Replaces the current region with the output of the given shell command."
  (interactive "sShell command replace region: ")
  (let ((cmd (s-split " " command)))
    (cond
     ((s-blank? command) (message "Invalid argument"))
     ((not (executable-find (car cmd))) (message "Command '%s' not found" (car cmd)))
     ((not (use-region-p)) (message "No region was active"))
     (t (shell-command-on-region (region-beginning) (region-end) command nil t)))))

(defun slick-cut (beg end)
  "Copy/cut entire line when no region is active.
  http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html"
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))
(advice-add 'kill-region :before #'slick-cut)
(defun slick-copy (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))
(advice-add 'kill-ring-save :before #'slick-copy)

;; Fix Ctrl+Backspace
;; http://stackoverflow.com/questions/28221079#answer-39438119
(defun backward-kill-word-fixed ()
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword
                       (s-contains? " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))))

(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character.
  https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop"
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

(defun new-empty-buffer ()
  "Create a new empty buffer without prompting for a name.
  http://ergoemacs.org/emacs/emacs_new_empty_buffer.html"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

(defun align-values (start end)
  "Vertically aligns region based on lengths of the first value of each line.
  https://www.reddit.com/r/emacs/comments/69w9wg/can_we_do_this_in_emacs/dh9vra8/"
  (interactive "r")
  (align-regexp start end "\\S-+\\(\\s-+\\)" 1 1 nil))

(defun browse-emacs-d ()
  "Open .emacs.d in Dired."
  (interactive)
  (dired user-emacs-directory))
