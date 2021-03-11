(use-package scala-mode
  :straight t
  :defer t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sc\\'"    . scala-mode)
         ("\\.sbt\\'"   . scala-mode))
  :init
  (defface scala-font-lock:constant-face
    '((t
       :foreground "#81a1c1"
       :weight bold))
    "Face used to constants in scala-mode."
    :group 'scala)
  (defvar scala-font-lock:constant-face
    'scala-font-lock:constant-face)
  (defface scala-font-lock:number-face
    '((t
       :foreground "#a3be8c"))
    "Face used to highlight numbers in scala-mode."
    :group 'scala)
  (defvar scala-font-lock:number-face
    'scala-font-lock:number-face)
  (defface scala-font-lock:brackets-face
    '((t
       :foreground "#89ddf7"))
    "Face used to highlight brackets in scala-mode."
    :group 'scala)
  (defvar scala-font-lock:brackets-face
    'scala-font-lock:brackets-face)
  (defface scala-font-lock:parentheses-face
    '((t
       :foreground "#89ddf7"))
    "Face used to highlight parenthesess in scala-mode."
    :group 'scala)
  (defvar scala-font-lock:parentheses-face
    'scala-font-lock:parentheses-face)
  (defface scala-font-lock:braces-face
    '((t
       :foreground "#89ddf7"))
    "Face used to highlight braces in scala-mode."
    :group 'scala)
  (defvar scala-font-lock:braces-face
    'scala-font-lock:parentheses-face)
  (setq scala-indent:use-javadoc-style t)

  (defun config/flycheck-scala-set-scalastylerc ()
    (let* ((stylelintrc "scalastyle.xml")
           (candidates (list (concat (file-name-as-directory (or (projectile-project-p) "")) stylelintrc)
                             (expand-file-name (concat "~/" stylelintrc)))))
      (setq-local flycheck-scalastylerc (--first (file-exists-p it) candidates))))

  (defun config/scala-enable-fill-column ()
    (let* ((scalafmt-conf (concat (file-name-directory (projectile-project-p (buffer-file-name)))
                                  ".scalafmt.conf"))
           (rule-column (or (when (file-exists-p scalafmt-conf)
                              (when-let ((max-column
                                          (with-temp-buffer
                                            (insert-file-contents scalafmt-conf)
                                            (when (re-search-forward "maxColumn[\\t ]*=[\\t ]*" nil t)
                                              (let ((p (point)))
                                                (end-of-line)
                                                (buffer-substring p (point)))))))
                                (string-to-number (s-trim max-column))))
                          130)))
      (setq-local fci-rule-column rule-column)
      (turn-on-fci-mode)))

  (defun config/scala-mode-init ()
    (setq-local company-minimum-prefix-length 0)
    (rainbow-delimiters-mode 1)
    (yas-minor-mode 1)
    (eldoc-mode -1)
    (config/flycheck-scala-set-scalastylerc))
  (add-hook 'scala-mode-hook #'config/scala-mode-init)

  (defun config/scala-add-package-to-new-file ()
    (let* ((path (buffer-file-name))
           (ext (file-name-extension path)))
      (when (and (member ext '("scala" "sc"))
                 (= (point-min) (point-max)))
        (let* ((slice (cadr (s-slice-at "src/main" (file-name-directory path)))))
          (when (s-starts-with? "src/main" slice)
            (let ((parts (--filter (not (s-blank? it)) (-drop 2 (s-split "/" slice)))))
              (when (s-matches? "scala.*" (car parts))
                (setq parts (cdr parts)))
              (insert (format "package %s" (s-join "." parts)))
              (newline 2)))))))
  (add-hook 'find-file-hook #'config/scala-add-package-to-new-file)

  (defun config/scala-after-company-complete (completion)
    "Remove the empty parens from Java-style accessor methods when completing."
    (when (and  (stringp completion)
                (eq major-mode 'scala-mode)
                (s-matches-p "\\(get\\)\\|\\(is\\)[A-Z]?" completion)
                (looking-back "()"))
      (delete-backward-char 2)))
  (add-hook 'company-after-completion-hook #'config/scala-after-company-complete)

  :config
  (setq scala-auto-insert-asterisk-in-comments t)

  (defun config/scala-newline-and-indent ()
    (interactive)
    (newline-and-indent)
    (when (save-excursion
            (beginning-of-line)
            (looking-at "[\t ]*}[\t ]*"))
      (previous-line)
      (end-of-line)
      (newline-and-indent))
    (when (save-excursion
            (next-line)
            (looking-at "[\t ]+\\."))
      (let ((indentation (save-excursion
                           (next-line)
                           (beginning-of-line)
                           (let ((pos (point)))
                             (search-forward ".")
                             (- (point) pos 1)))))
        (scala-indent:indent-line-to indentation)))
    (when scala-auto-insert-asterisk-in-comments
      (scala-indent:insert-asterisk-on-multiline-comment)))

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*ENSIME-Compilation-Result*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height   . 0.20)))

  (general-define-key
   :keymaps 'scala-mode-map
   :states 'insert
   "<return>" 'config/scala-newline-and-indent))

(provide 'config-scala)
