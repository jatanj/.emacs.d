;; init.el -- Emacs configuration

;; Bootstrap use-package.
;; http://stackoverflow.com/questions/10092322#answer-10093312
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Load machine-specific settings
(let ((local-settings "~/.emacs.d/local.el"))
  (when (file-exists-p local-settings) (load local-settings)))
(dolist (local-setting '((local-directory . "~/")
                         (local-terminal . nil)
                         (local-font-face . "Inconsolata-12")
                         (local-default-theme . "evening-dark")
                         (local-desktop-window-params . nil)
                         (local-client-window-params . nil)))
  (unless (boundp (car local-setting))
    (set (car local-setting) (cdr local-setting))))

; Add site-list directories to load-path
(dolist
    (package (directory-files (concat user-emacs-directory "site-lisp") t "\\w+"))
  (when (file-directory-p package)
    (add-to-list 'load-path package)))

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
(setq x-gtk-use-system-tooltips nil)
(setq isearch-allow-scroll t)
(setq load-prefer-newer t)
(setq-default next-line-add-newlines nil)
(setq save-interprogram-paste-before-kill t)
(setq w32-pipe-read-delay 0)
(setq ad-redefinition-action 'accept)
(setq inhibit-compacting-font-caches t)

(setq configure-frame-functions '())
(setq configure-display-buffer-alist '())

;; Useful minor modes
(show-paren-mode 1)
(global-superword-mode 1)
(global-auto-revert-mode 1)

;; Window size and position
(setq default-frame-alist
      (if (daemonp)
          local-client-window-params
        local-desktop-window-params))
(setq resize-mini-windows t)
(setq even-window-heights nil)

;; Minimize frame if it was saved as maximized
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

;; Save desktop
(unless (daemonp)
  (setq desktop-restore-eager t)
  (desktop-save-mode)
  (dolist (no-save-mode '(magit-mode
                          magit-log-mode))
    (add-to-list 'desktop-modes-not-to-save no-save-mode))
  (dolist (no-save-files '("\\`COMMIT_EDITMSG"))
    (add-to-list 'desktop-files-not-to-save no-save-files)))

;; Set font
(add-to-list 'default-frame-alist `(font . ,local-font-face))
(set-face-attribute 'default nil :font local-font-face)

;; Cursor
(blink-cursor-mode -1)
(save-place-mode 1)
(setq-default cursor-in-non-selected-windows nil)

;; Line numbers
(global-linum-mode 1)
(setq linum-format "%4d ")
(setq-default left-fringe-width 10)

;; Vertical scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-error-top-bottom t)
(add-hook 'configure-frame-functions
  (lambda (frame)
    (scroll-bar-mode -1)
    (horizontal-scroll-bar-mode -1)))

;; Horizontal scrolling
(setq hscroll-step 1)
(setq hscroll-margin 1)
(defun toggle-hscroll-mode (&optional arg global)
  (let ((scroll-vars '(auto-hscroll-mode truncate-lines))
        (set-value (lambda (scroll-var value &optional global)
                     (let ((result (pcase value
                                     ((pred (booleanp)) (if value value (not (symbol-value scroll-var))))
                                     ((pred (numberp)) (> value 0))
                                     (_ nil))))
                       (if global
                           (set-default scroll-var result)
                         (set scroll-var result))))))
    (unless global (make-local-variable 'auto-hscroll-mode))
    (dolist (val scroll-vars)
      (funcall set-value val arg global))))
(defun global-hscroll-mode (&optional arg)
  (interactive)
  (toggle-hscroll-mode arg t))
(defun hscroll-mode (&optional arg)
  (interactive)
  (toggle-hscroll-mode arg))
(global-hscroll-mode 1)

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

;; Searching
;; http://emacs.stackexchange.com/questions/10307/#answer-10432
(defun isearch-center-cursor (&rest _)
  (sit-for 0)
  (if (and
       ;; not the scrolling command
       (not (eq this-command 'isearch-other-control-char))
       ;; not the empty string
       (> (length isearch-string) 0)
       ;; not the first key (to lazy highlight all matches w/o recenter)
       (> (length isearch-cmds) 2)
       ;; the point in within the given window boundaries
       (let ((line (count-screen-lines (point) (window-start))))
         (or (> line (* (/ (window-height) 4) 3))
             (< line (* (/ (window-height) 9) 1)))))
      (let ((recenter-position 0.3))
        (recenter '(4)))))
(advice-add 'isearch-update :before #'isearch-center-cursor)
(advice-add 'evil-search :after #'isearch-center-cursor)

;; Use UTF-8 everywhere
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
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

;; Create a new empty buffer without prompting for a name.
;; http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
(defun new-empty-buffer ()
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

;; https://www.reddit.com/r/emacs/comments/69w9wg/can_we_do_this_in_emacs/dh9vra8/"
(defun align-values (start end)
  "Vertically aligns region based on lengths of the first value of each line."
  (interactive "r")
  (align-regexp start end
                "\\S-+\\(\\s-+\\)"
                1 1 nil))

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
    (funcall indent-line-function)))

(defun open-terminal-here ()
  "Opens a terminal window in the current buffer's directory."
  (interactive)
  (when local-terminal
    (let* ((buffer-directory (file-name-directory (file-truename (buffer-file-name))))
           (args (pcase local-terminal
                   ("xfce4-terminal" `("--default-working-directory" ,buffer-directory))
                   (_ nil))))
      (apply 'call-process (executable-find local-terminal) nil 0 nil args))))

;; Fixes Gnus vulnerability
(eval-after-load "enriched" '(defun enriched-decode-display-prop (start end &optional param) (list start end)))

(setq leader-key "C-l")
(global-set-key (kbd leader-key) nil)

(use-package general :ensure t :demand)
(use-package hydra :ensure t :demand)
(use-package dash :ensure t :demand)

;; Dired
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook #'hl-line-mode)

;; DocView
(add-hook 'doc-view-mode-hook (lambda () (linum-mode -1)))

;; Help
;; (setq help-window-select t)
(add-to-list 'configure-display-buffer-alist
             '("\\`\\*Help\\*\\'" help-mode))

;; Load config files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/config"))
(dolist (name '(all-the-icons
                anzu
                beacon
                c-cpp
                clojure
                company
                d
                dumb-jump
                emacs-lisp
                evil
                expand-region
                fill-column-indicator
                flycheck
                flyspell
                fsharp
                haskell
                helm
                ibuffer-projectile
                ido
                java
                javascript
                json
                litable
                magit
                markdown
                neotree
                org
                projectile
                rainbow-mode
                ranger
                rust
                scala
                screenshow-mode
                shell
                smartparens
                smooth-scroll
                spaceline
                tabbar
                term
                typescript
                uniquify
                web-mode
                which-key
                window-numbering
                winner))
  (require (intern (concat "config-" (symbol-name name)))))

;; Themes
(setq custom-theme-directory "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(defun switch-theme (&optional name)
  (interactive)
  (let* ((available-themes (custom-available-themes))
         (name (or name (completing-read "Switch to theme: " available-themes)))
         (theme (intern name)))
    (if (member theme available-themes)
        (progn (load-theme theme t)
               (setq custom-current-theme name))
      (message "Could not find theme '%s'" theme))))
(defun load-initial-theme ()
  (switch-theme (or (and (boundp 'custom-current-theme)
                         custom-current-theme)
                    local-default-theme)))
(if (bound-and-true-p desktop-save-mode)
    (progn
      (add-to-list 'desktop-globals-to-save 'custom-current-theme)
      (dolist (hook '(desktop-after-read-hook
                      desktop-not-loaded-hook
                      desktop-no-desktop-file-hook))
        (add-hook hook #'load-initial-theme)))
  (add-hook 'configure-frame-functions (lambda (frame) (load-initial-theme))))

;; If we're using emacslient, we need to delay all configure-frame-functions
;; until after the frame is created.
(defun configure-frame (frame)
  (dolist (func configure-frame-functions)
    (funcall func frame))
  (redraw-frame frame))
(if (daemonp)
    (add-hook 'after-make-frame-functions
      (lambda (frame)
        (configure-frame frame)))
  (configure-frame (selected-frame)))

;; Configure how these buffers are displayed and add some shortcuts to quickly
;; close them
(dolist (it configure-display-buffer-alist)
  (pcase it
    (`(,regexp ,mode)
     (general-define-key
      :keymaps (intern (format "%s-map" mode))
      "q" 'kill-this-buffer
      "C-g" 'kill-this-buffer)
     (add-hook (intern (format "%s-hook" mode))
               (lambda ()
                 (switch-to-buffer-other-window (current-buffer))
                 (redisplay t)))
     (add-to-list 'display-buffer-alist
                  `(,regexp
                    (display-buffer-in-side-window)
                    (inhibit-same-window . t)
                    (side . bottom)
                    (slot . 1)
                    (window-height . 0.30))))))

;; Unbind some keys
(dolist (key '("M-<DEL>" "M-`" "M-u" "M-i" "M-o" "M-p" "M-k" "M-l" "M-m" "M-:" "M-/"
               "M-<lwindow>" "C-<lwindow>"))
  (global-set-key (kbd key) 'ignore))
(general-define-key
 :keymaps 'undo-tree-map
 "C-/" nil
 "C-_" nil)

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
 "C-p" 'helm-projectile-find-file
 "C-<backspace>" 'backward-kill-word-fixed
 "C-S-<backspace>" 'backspace-whitespace-to-tab-stop
 "C-<tab>" 'previous-buffer
 "<C-iso-lefttab>" 'next-buffer
 "C-/" 'comment-line-or-region
 "C-\\" 'indent-line-or-region
 "C-|" 'sort-lines
 "C-M-\\" 'align-values
 "<prior>" 'evil-scroll-up
 "<next>" 'evil-scroll-down
 "C-<prior>" 'tabbar-backward-tab
 "C-<next>" 'tabbar-forward-tab
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
 "`" 'open-terminal-here
 "w" 'kill-this-buffer
 "p" 'helm-projectile-find-file-in-known-projects
 "k" 'ido-kill-buffer
 "f" 'helm-find
 "b" 'helm-buffers-list
 "m" (general-simulate-keys "C-c")
 "!" 'new-empty-buffer
 "C-u" nil ; upcase-region
 "C-l" nil ; downcase-region
 "C-d" 'ido-dired
 "C-p" 'helm-projectile-find-file-in-known-projects
 "C-v" 'magit-status
 "C-b" nil
 "C-h" 'hscroll-mode)
(general-define-key
 :prefix leader-key
 "p" projectile-command-map
 "b" 'ibuffer
 "s" helm-swoop-command-map
 "C-a" 'neotree-find
 "C-b" 'neotree-projectile
 "C-n" 'neotree-switch-to-project-root
 "v" 'magit-file-popup
 "C-v" 'magit-status)
(general-define-key
 :keymaps '(fundamental-mode-map text-mode-map special-mode-map)
 "C-d" (general-simulate-keys "<next>")
 "C-u" (general-simulate-keys "<prior>"))
(general-define-key
 :keymaps 'isearch-mode-map
 "C-f" 'isearch-repeat-forward
 "C-h" 'isearch-query-replace-regexp
 "C-w" 'helm-swoop-from-isearch
 "<up>" 'isearch-ring-retreat
 "<down>" 'isearch-ring-advance)

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
    (git-gutter window-numbering which-key web-mode use-package ts-comint tide tabbar spaceline smooth-scroll smartparens org-bullets neotree markdown-mode json-mode js2-mode iflipb ido-vertical-mode ibuffer-projectile hydra helm-projectile helm-ag gitignore-mode gitconfig-mode general flycheck-haskell flx-ido expand-region evil-visualstar evil-surround evil-matchit evil-magit esup ensime d-mode clojure-mode-extra-font-locking cider anzu all-the-icons))))
