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
  (add-hook 'scala-mode-hook (lambda () (rainbow-delimiters-mode 1)))

  :config
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
   :keymaps 'scala-mode-map
   :states 'insert
   "<return>" 'scala-newline-and-indent-with-asterisk))

(provide 'config-scala)
