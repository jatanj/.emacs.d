(use-package ensime
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

  :config
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  (setq ensime-implicit-gutter-icons nil)
  (setq ensime-left-margin-gutter nil)
  (setq ensime-sbt-perform-on-save "compile")
  (setq ensime-typecheck-interval 1)
  (setq ensime-typecheck-idle-interval 0.2)
  (setq ensime-use-helm t)
  (setq ensime-company-idle-delay 0.1)
  (setq ensime-eldoc-hints 'error)
  (setq ensime-sem-high-enabled-p nil)
  (setq scala-auto-insert-asterisk-in-comments t)

  (defun scala-newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (when scala-auto-insert-asterisk-in-comments
      (scala-indent:insert-asterisk-on-multiline-comment)))

  (add-to-list 'display-buffer-alist
               `(,(rx bos "*ENSIME-Compilation-Result*" eos)
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (window-height   . 0.20)))

  (general-define-key
   :keymaps 'ensime-mode-map
   "C-c C-]" 'ensime-edit-definition
   "C-c C-k" 'ensime-inf-eval-buffer
   "C-c C-s" 'ensime-print-errors-at-point)

  (general-define-key
   :keymaps 'scala-mode-map
   :states 'insert
   "<return>" 'scala-newline-and-indent-with-asterisk))

(use-package flycheck-ensime
  :after ensime
  :config
  (add-hook 'ensime-mode-hook
            (lambda ()
              (flycheck-ensime-setup)
              (flycheck-mode 1))))

(provide 'config-scala)
