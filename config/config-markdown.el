(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'turn-on-orgtbl)
  (dolist (mode '(gfm-mode markdown-mode))
    (add-hook (intern (format "%s-hook" mode))
              (lambda ()
                (turn-on-flyspell-if-ispell-exists)
                (turn-on-proselint))))
  :config
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
  (ignore-errors (require 'livedown))
  (setq markdown-gfm-use-electric-backquote nil)
  (setq markdown-asymmetric-header t)
  (setq markdown-enable-math t)
  (setq markdown-use-pandoc-style-yaml-metadata t)
  (defhydra hydra-markdown-rank (markdown-mode-map "C-c")
    ("C--" markdown-promote "Promote")
    ("C-=" markdown-demote "Demote"))
  (defhydra hydra-gfm-rank (gfm-mode-map "C-c")
    ("C--" markdown-promote "Promote")
    ("C-=" markdown-demote "Demote"))
  (general-define-key
   :keymaps '(markdown-mode-map gfm-mode-map)
   "C-c C-k" 'livedown-preview
   "C-c C-q" 'livedown-kill
   "C-c C-p" (lambda () (interactive) (markdown-pandoc-convert-buffer 'pdf))
   "C-c C-n" (lambda () (interactive) (markdown-pandoc-convert-buffer 'odt))
   "C-c C-<up>" 'markdown-previous-heading
   "C-c C-<down>" 'markdown-next-heading
   "M-<up>" nil
   "M-<down>" nil
   "M-<left>" nil
   "M-<right>" nil))

(defvar markdown-pandoc-output-format 'pdf
  "The output document format to convert to when invoking pandoc.")

(defun markdown--pandoc-convert-to-format (input-format output-format)
  (let ((file (file-truename (buffer-file-name)))
        (shell (lambda (command) (call-process-shell-command command nil 0))))
    (pcase (list input-format output-format)
      (`(markdown odt) (funcall shell (format "md2odt %s" file)))
      (`(markdown pdf) (funcall shell (format "md2pdf %s" file))))))

(defun markdown-pandoc-convert-buffer (&optional output-format)
  (interactive)
  (let ((input-format (pcase (file-name-extension (buffer-file-name))
                        ("md" 'markdown)
                        ("markdown" 'markdown)))
        (output-format (or output-format markdown-pandoc-output-format)))
    (when (and input-format output-format)
      (markdown--pandoc-convert-to-format input-format output-format))))

(define-minor-mode markdown-pandoc-mode
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-p") 'markdown-pandoc-convert-buffer)
            map)
  :group 'markdown
  (if (bound-and-true-p markdown-pandoc-mode)
      (progn
        (make-local-variable 'markdown-pandoc-output-format)
        (add-hook 'after-save-hook 'markdown-pandoc-convert-buffer))
    (remove-hook 'after-save-hook 'markdown-pandoc-convert-buffer)))

(defun enable-markdown-pandoc-mode (output-format)
  (markdown-pandoc-mode 1)
  (message (format "Converting to %s on save..." (upcase (symbol-name output-format))))
  (setq markdown-pandoc-output-format output-format))

(defun enable-markdown-pandoc-mode-pdf () (interactive) (enable-markdown-pandoc-mode 'pdf))
(defun enable-markdown-pandoc-mode-odt () (interactive) (enable-markdown-pandoc-mode 'odt))

(provide 'config-markdown)
