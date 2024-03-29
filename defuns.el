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

;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;; Smart Ctrl+Backspace
;; http://stackoverflow.com/questions/28221079#answer-39438119
(defun backward-delete-word-smart ()
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
              (delete-region cp end)
            (if space-pos
                (delete-region cp space-pos)
              (backward-delete-word 1))))
      (delete-region cp (- cp 1)))))

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

;; Searching
;; http://emacs.stackexchange.com/questions/10307/#answer-10432
(defun isearch-center-cursor (&rest _)
  (sit-for 0)
  (if (and
       (not (eq this-command 'isearch-other-control-char))
       (> (length isearch-string) 0)
       (> (length isearch-cmds) 2)
       (let ((line (count-screen-lines (point) (window-start))))
         (or (> line (* (/ (window-height) 4) 3))
             (< line (* (/ (window-height) 9) 1)))))
      (let ((recenter-position 0.3))
        (recenter '(4)))))
(advice-add 'isearch-update :before #'isearch-center-cursor)
(advice-add 'evil-search :after #'isearch-center-cursor)

;; Splitting
;; https://emacs.stackexchange.com/a/40517
(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Based on split-window-sensibly, but designed to prefer a horizontal split,
  i.e. windows tiled side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically
             (with-selected-window window
               (split-window-below)))
        (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
         (not (window-minibuffer-p window))
         (let ((split-width-threshold 0))
           (when (window-splittable-p window t)
             (with-selected-window window
               (split-window-right))))))))
