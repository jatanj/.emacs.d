;; init.el -- Emacs configuration

;; Packages
(require 'package)
(setq package-list '(all-the-icons
                     anzu
                     cider
                     clojure-mode
                     clojure-mode-extra-font-locking
                     company
                     dash
                     d-mode
                     ensime
                     esup
                     evil
                     evil-magit
                     evil-matchit
                     evil-surround
                     evil-visualstar
                     expand-region
                     flx
                     flx-ido
                     flycheck-haskell
                     fsharp-mode
                     general
                     ggtags
                     gitconfig-mode
                     gitignore-mode
                     haskell-mode
                     helm
                     helm-ag
                     helm-projectile
                     hydra
                     ibuffer-projectile
                     ido-vertical-mode
                     iflipb
                     js2-mode
                     json-mode
                     magit
                     markdown-mode
                     neotree
                     omnisharp
                     org-bullets
                     popwin
                     projectile
                     smartparens
                     smooth-scroll
                     spaceline
                     tabbar
                     tide
                     ts-comint
                     use-package
                     web-mode
                     which-key
                     window-numbering))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Install missing packages
;; http://stackoverflow.com/questions/10092322#answer-10093312
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load machine-specific settings
(let ((local-settings "~/.emacs.d/local.el"))
  (when (file-exists-p local-settings) (load local-settings)))
(dolist (local-setting '((default-dir               . "~/")
                         (custom-font-face          . "Inconsolata-12")
                         (desktop-window-attributes . nil)
                         (client-window-attributes  . nil)))
  (unless (boundp (car local-setting))
    (set (car local-setting) (cdr local-setting))))

; Add site-list directories to load-path
(dolist
    (project (directory-files (concat user-emacs-directory "site-lisp") t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Startup options
(setq frame-title-format "Emacs - %b")
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message () (message ""))
(menu-bar-mode -1)
(tool-bar-mode -1)

;; General config
(setq-default major-mode 'text-mode)
(setq tooltip-use-echo-area t)
(setq isearch-allow-scroll t)
(setq load-prefer-newer t)
(setq even-window-heights nil)
(setq-default next-line-add-newlines nil)
(setq save-interprogram-paste-before-kill t)
(setq w32-pipe-read-delay 0)
(setq ad-redefinition-action 'accept)
(show-paren-mode 1)
(global-superword-mode 1)
(global-auto-revert-mode 1)

;; Set the window position on startup
(setq configure-frame-functions '())
(if (daemonp)
  (setq default-frame-alist client-window-attributes)
  (progn
    (desktop-save-mode 1)
    (setq default-frame-alist desktop-window-attributes)))
(add-hook 'desktop-after-read-hook
          (lambda ()
            (cond ((string= (frame-parameter nil 'fullscreen) 'maximized)
            (toggle-frame-maximized))
            ((string= (frame-parameter nil 'fullscreen) 'fullboth)
            (toggle-frame-fullscreen) (toggle-frame-maximized)))))

;; Fix toggle-frame-fullscreen to preserve our window position
(defun force-maximized-with-fullscreen (orig-fun &rest args)
  (let ((fullscreen-parameter (frame-parameter nil 'fullscreen)))
    (unless (or (string= fullscreen-parameter 'fullboth)
		(string= fullscreen-parameter 'maximized))
      (toggle-frame-maximized)))
  (apply orig-fun args))
(advice-add 'toggle-frame-fullscreen :around #'force-maximized-with-fullscreen)

;; Set font
(add-to-list 'default-frame-alist `(font . ,custom-font-face))
(set-face-attribute 'default nil :font custom-font-face)

;; Customize cursor
(blink-cursor-mode -1)
(save-place-mode 1)
(setq-default cursor-in-non-selected-windows nil)

;; Uniquify buffer names
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
;; (setq uniquify-buffer-name-style 'forward)
;; (setq uniquify-separator "/")

;; Line numbers
(global-linum-mode 1)
(setq linum-format "%4d ")
(setq resize-mini-windows t)

;; Vertical scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-error-top-bottom t)
(add-to-list 'configure-frame-functions
  (lambda ()
    (scroll-bar-mode -1)
    (horizontal-scroll-bar-mode -1)))

;; Horizontal scrolling
(setq hscroll-step 1)
(setq hscroll-margin 1)
(setq-default truncate-lines t)
(defun toggle-horizontal-scrolling ()
  (interactive)
  (let ((config-vars '(auto-hscroll-mode truncate-lines)))
    (if (bound-and-true-p auto-hscroll-mode)
        (dolist (var config-vars) (set var nil))
      (dolist (var config-vars) (set var t)))))

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-stop-list (number-sequence 2 200 2))
(setq-default indent-line-function 'insert-tab)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defun set-local-tab-width (n)
  (setq tab-width n)
  (setq evil-shift-width n)
  (set (make-local-variable 'tab-stop-list) (number-sequence n 200 n)))

;; Use UTF-8 everywhere
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16le-dos 'utf-8))

;; Backup files
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Copy/cut entire line when no region is active
;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defun slick-cut (beg end)
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

;; Backspace previous tab stop
;; https://www.emacswiki.org/emacs/BackspaceWhitespaceToTabStop
(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
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

;; Configure packages
(setq config-packages '(general
                        evil
                        helm
                        projectile
                        magit
                        window-numbering
                        anzu
                        company
                        flycheck
                        tabbar
                        neotree
                        smartparens
                        smooth-scroll
                        expand-region
                        spaceline
                        ispell
                        popwin
                        which-key
                        iflipb
                        ido
                        ibuffer
                        term
                        org
                        ;; yasnippets
                        web-mode
                        emacs-lisp
                        markdown
                        c-cpp
                        java
                        d
                        csharp
                        typescript
                        javascript
                        scala
                        fsharp
                        clojure
                        haskell))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config"))
(dolist (name config-packages)
  (require (intern (concat "config-" (symbol-name name)))))

;; Load theme
(setq custom-theme-directory "~/.emacs.d/themes/")
(add-to-list 'configure-frame-functions (lambda () (load-theme 'custom-dark t)))

;; Set fonts and other stuff when using emacsclient
(defun configure-frame ()
  (dolist (func configure-frame-functions)
    (funcall func))
  (redraw-frame))
(if (daemonp)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (select-frame frame)
      (configure-frame)))
  (configure-frame))

;; Unbind some keys
(dolist
    (key '("M-<DEL>" "M-`" "M-u" "M-i" "M-o" "M-p" "M-k" "M-l" "M-m" "M-:" "M-/"
           "M-<lwindow>" "C-<lwindow>"))
  (global-set-key (kbd key) 'ignore))

;; Keybindings
(general-define-key
 "C-k" ctl-x-map
 "C-;" 'execute-extended-command
 "C-:" 'eval-expression
 "M-<f4>" (if (daemonp) 'delete-frame 'save-buffers-kill-emacs)
 "C-=" 'enlarge-window-horizontally
 "C--" 'shrink-window-horizontally
 "C-+" 'enlarge-window
 "C-_" 'shrink-window
 "C-S-p" 'helm-M-x
 "C-p" 'helm-buffers-list
 "C-<backspace>" 'backward-kill-word-fixed
 "C-S-<backspace>" 'backspace-whitespace-to-tab-stop
 "C-\\" 'indent-region
 "<prior>" 'evil-scroll-up
 "<next>" 'evil-scroll-down
 "C-<prior>" 'tabbar-backward-tab
 "C-<next>" 'tabbar-forward-tab
 "C-S-<prior>" 'iflipb-next-buffer
 "C-S-<next>" 'iflipb-previous-buffer
 "M-<left>" 'windmove-left
 "M-<right>" 'windmove-right
 "M-<up>" 'windmove-up
 "M-<down>" 'windmove-down
 "S-<up>" (lambda () (interactive) (previous-line 10))
 "S-<down>" (lambda () (interactive) (next-line 10))
 "S-<left>" (lambda () (interactive) (backward-char 10))
 "S-<right>" (lambda () (interactive) (forward-char 10)))

(general-define-key
 :keymaps 'ctl-x-map
 "w" 'kill-this-buffer
 "p" 'helm-projectile-find-file-in-known-projects
 "k" 'ido-kill-buffer
 "f" 'ido-find-file
 "b" 'helm-buffers-list
 "m" (general-simulate-keys "C-c")
 "C-p" 'helm-projectile-find-file-in-known-projects
 "C-v" 'magit-status
 "C-b" 'neotree-projectile
 "C-h" 'toggle-horizontal-scrolling)

(general-define-key
 :prefix leader-key
 "p" projectile-command-map
 "v" 'magit-status
 "b" 'ibuffer)

(general-define-key
 :keymaps '(fundamental-mode-map text-mode-map special-mode-map)
 "C-d" (general-simulate-keys "<next>")
 "C-u" (general-simulate-keys "<prior>"))

(general-define-key
 :keymaps 'isearch-mode-map
 "C-f" 'isearch-repeat-forward
 "C-h" 'isearch-query-replace-regexp)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (smartparens cider clojure-mode-extra-font-locking clojure-mode esup smooth-scroll anzu ensime expand-region window-numbering which-key evil-surround tide web-mode use-package typescript-mode tabbar spaceline neotree magit js2-mode ido-vertical-mode helm-projectile helm-ag general flycheck flx-ido evil company))))
