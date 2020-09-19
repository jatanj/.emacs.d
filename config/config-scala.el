(use-package scala-mode
  :ensure t
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

  (defun config/scala-mode-init ()
    (rainbow-delimiters-mode 1)
    (yas-minor-mode 1)
    (eldoc-mode -1)
    ;; Metals only seems to check for errors after a save, so we'll save often.
    (when (bound-and-true-p super-save-mode)
      (setq-local super-save-idle-duration 1)))
  (add-hook 'scala-mode-hook #'config/scala-mode-init)

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
