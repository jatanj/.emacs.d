;; init.el -- Emacs configuration

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

(defun config/user-emacs-path (name)
  (interactive)
  (expand-file-name name user-emacs-directory))

(defconst init-file-path (config/user-emacs-path "init.el"))
(defconst init-themes-path (config/user-emacs-path "themes"))
(defconst init-config-path (config/user-emacs-path "config"))
(defconst init-site-lisp-path (config/user-emacs-path "site-lisp"))
(defconst init-local-path (config/user-emacs-path "local.el"))
(defconst init-custom-path (config/user-emacs-path "custom.el"))
(defconst init-defuns-path (config/user-emacs-path "defuns.el"))
(defconst init-config-name-prefix "config-")

;; Load machine-specific settings
(when (file-exists-p init-local-path)
  (load init-local-path))
(dolist (local-setting '((local-directory . "~/")
                         (local-terminal . nil)
                         (local-font-face . "Inconsolata-12")
                         (local-menu-font-face . "Fira Code Medium-10")
                         (local-default-theme . "nord")
                         (local-terminals . '("xterm"))
                         (local-desktop-window-params . nil)
                         (local-client-window-params . nil)))
  (unless (boundp (car local-setting))
    (set (car local-setting) (cdr local-setting))))

;; Emacs 28 temporary fixes
(setq minibuffer-local-must-match-filename-map (make-sparse-keymap))
(setq browse-url-mosaic-program nil)

;; Configuration dependencies
(use-package general :straight t :demand)
(use-package hydra :straight t :demand)
(use-package dash :straight t :demand)
(use-package s :straight t :demand)
(use-package ht :straight t :demand)
(use-package switch-buffer-functions :straight t :demand)

(load init-defuns-path)

;; Set up load path
(when (file-directory-p init-site-lisp-path)
  (dolist (package (directory-files init-site-lisp-path t "\\w+"))
    (when (file-directory-p package)
      (add-to-list 'load-path package))))

;; Keep custom settings in separate file
(setq custom-file init-custom-path)
(when (file-exists-p custom-file)
  (load custom-file))

;; Startup options
(setq frame-title-format "Emacs - %b")
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message () (message ""))
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(global-eldoc-mode -1)

;; General configuration
(setq-default major-mode 'text-mode)
(setq x-gtk-use-system-tooltips nil)
(setq isearch-allow-scroll t)
(setq load-prefer-newer t)
(setq-default next-line-add-newlines nil)
(setq save-interprogram-paste-before-kill t)
(setq w32-pipe-read-delay 0)
(setq ad-redefinition-action 'accept)
(setq inhibit-compacting-font-caches t)
(setq delete-by-moving-to-trash t)
(setq completion-styles '(flex))
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.02)
(setq inhibit-compacting-font-caches t)
(setq fci-rule-width 1)
(setq-default line-spacing 0)
(setq warning-minimum-level :emergency)
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(setq config/configure-frame-functions '())
(setq config/quick-kill-buffer-list '())
(setq config/save-keybind-hook '())

;; Useful minor modes
(show-paren-mode 1)
(global-superword-mode 1)
(global-auto-revert-mode 1)
(column-number-mode 1)

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
(defun config/force-maximized-with-fullscreen (orig-fun &rest args)
  (let ((fullscreen-parameter (frame-parameter nil 'fullscreen)))
    (unless (or (string= fullscreen-parameter 'fullboth)
                (string= fullscreen-parameter 'maximized))
      (toggle-frame-maximized)))
  (apply orig-fun args))
(advice-add 'toggle-frame-fullscreen :around #'config/force-maximized-with-fullscreen)

;; Save desktop
(setq desktop-restore-eager t)
(setq desktop-load-locked-desktop t)
(desktop-save-mode 1)
(if (daemonp)
    (setq desktop-restore-frames nil))
(dolist (no-save-mode '(magit-mode
                        magit-log-mode))
  (add-to-list 'desktop-modes-not-to-save no-save-mode))
; (dolist (f '("\\`COMMIT_EDITMSG"))
;   (add-to-list 'desktop-files-not-to-save f))

;; Set font
(add-to-list 'default-frame-alist `(font . ,local-font-face))
(set-face-attribute 'default nil :font local-font-face)

;; Cursor
(blink-cursor-mode -1)
(save-place-mode 1)
(setq-default cursor-in-non-selected-windows nil)

;; Line numbers
(linum-mode -1)
(setq-default left-fringe-width 10)
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers t)
(setq-default display-line-numbers-grow-only t)
(setq-default display-line-numbers-width-start t)

;; Vertical scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-error-top-bottom t)
(add-hook 'config/configure-frame-functions
  (lambda (frame)
    (scroll-bar-mode -1)
    (horizontal-scroll-bar-mode -1)))

;; Horizontal scrolling
(setq-default truncate-partial-width-windows nil)
(setq-default truncate-lines t)
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
(defun config/delete-trailing-whitespace ()
  (let ((end-previous-line (save-excursion
                             (previous-line)
                             (end-of-line)
                             (point)))
        (start-next-line (save-excursion
                           (next-line)
                           (beginning-of-line)
                           (point))))
    (delete-trailing-whitespace (point-min) end-previous-line)
    (delete-trailing-whitespace start-next-line (point-max))))
(add-hook 'before-save-hook 'config/delete-trailing-whitespace)
(defun config/set-local-tab-width (n)
  (setq tab-width n)
  (setq evil-shift-width n)
  (set (make-local-variable 'tab-stop-list) (number-sequence n 200 n)))
(defun config/split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))
(setq split-height-threshold 4)
(setq split-width-threshold 40)
(setq split-window-preferred-function 'config/split-window-really-sensibly)

;; Use UTF-8 everywhere
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16le-dos 'utf-8))

;; Tramp
(setq tramp-copy-size-limit nil)
(setq tramp-inline-compress-start-size nil)

;; Byte compilation
;; https://emacs.stackexchange.com/questions/13532/
(defun config/dont-delay-compile-warnings (fun type &rest args)
  (if (eq type 'bytecomp)
      (let ((after-init-time t))
        (apply fun type args))
    (apply fun type args)))
(advice-add 'display-warning :around #'config/dont-delay-compile-warnings)

;; Backup files
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq leader-key "C-l")
(global-set-key (kbd leader-key) nil)

;; File Templates
(auto-insert-mode 1)
(setq auto-insert t)
(setq auto-insert-directory (concat (file-name-as-directory user-emacs-directory) "file-templates"))
(setq auto-insert-query nil)
(defun auto-insert-yas-expand()
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))
(setq auto-insert-alist '())

;; Themes
(setq custom-theme-directory init-themes-path)
(add-to-list 'load-path init-themes-path)
(defun config/switch-theme (&optional name)
  (interactive)
  (let* ((available-themes (custom-available-themes))
         (name (or name (completing-read "Switch to theme: " available-themes)))
         (theme (intern name)))
    (if (member theme available-themes)
        (progn (load-theme theme t)
               (setq custom-current-theme name))
      (message "Could not find theme '%s'" theme))))
(defun config/load-initial-theme ()
  (config/switch-theme (or (and (boundp 'custom-current-theme)
                                custom-current-theme)
                           local-default-theme)))
(if (bound-and-true-p desktop-save-mode)
    (progn
      (add-to-list 'desktop-globals-to-save 'custom-current-theme)
      (dolist (hook '(desktop-after-read-hook
                      desktop-not-loaded-hook
                      desktop-no-desktop-file-hook))
        (add-hook hook #'config/load-initial-theme)))
  (add-hook 'config/configure-frame-functions
            (lambda (frame) (config/load-initial-theme))))

;; Load config files
(add-to-list 'load-path (expand-file-name init-config-path))
(dolist (name '(all-the-icons
                anzu
                apache
                beacon
                c-cpp
                centaur-tabs
                clojure
                company
                cql
                d
                dashboard
                docview
                doom
                dumb-jump
                emacs-lisp
                evil
                expand-region
                fennel
                fill-column-indicator
                flycheck
                flyspell
                fsharp
                haskell
                helm
                help
                highlight-numbers
                ibuffer-projectile
                ido
                indent-guides
                java
                javascript
                json
                litable
                lsp
                lua
                magit
                markdown
                org
                projectile
                quickrun
                rainbow-delimiters
                rainbow-mode
                ranger
                rust
                scala
                shell
                smartparens
                smooth-scroll
                sql
                super-save
                systemd
                term
                text
                treemacs
                typescript
                uniquify
                web
                which-key
                window-numbering
                winner
                xml
                yaml))
  (require (intern
            (concat init-config-name-prefix (symbol-name name)))))

;; Delay all configure-frame-functions for emacsclient until after the frame
;; is created.
(defun config/configure-frame (frame)
  (dolist (func config/configure-frame-functions)
    (funcall func frame))
  (redraw-frame frame))
(if (daemonp)
    (add-hook 'after-make-frame-functions
      (lambda (frame)
        (config/configure-frame frame)))
  (config/configure-frame (selected-frame)))

;; Configure how these buffers are displayed and add some shortcuts to quickly
;; close them
(dolist (it config/quick-kill-buffer-list)
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

(defun config/save-buffer ()
  (interactive)
  (dolist (func config/save-keybind-hook)
    (when (functionp func)
      (funcall func)))
  (save-buffer))

(defun config/reset-minibuffer ()
  (interactive)
  (set-window-buffer (minibuffer-window) (get-buffer " *Minibuf-0*")))

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
 "C-:" 'eval-expression
 "M-<f4>" (if (daemonp) 'delete-frame 'save-buffers-kill-emacs)
 "C-=" 'enlarge-window-horizontally
 "C--" 'shrink-window-horizontally
 "C-+" 'enlarge-window
 "C-_" 'shrink-window
 "C-S-p" 'helm-M-x
 "C-M-p" 'helm-M-x
 "C-'" 'helm-apropos
 "C-p" 'helm-projectile-find-file
 "C-S-s" 'save-some-buffers
 "C-b" nil
 "C-<backspace>" 'backward-delete-word-smart
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
 "C-<left>" 'windmove-left
 "C-<right>" 'windmove-right
 "C-<up>" 'windmove-up
 "C-<down>" 'windmove-down
 "S-<up>" (lambda () (interactive) (forward-line -10))
 "S-<down>" (lambda () (interactive) (forward-line 10))
 "S-<left>" (lambda () (interactive) (backward-char 10))
 "S-<right>" (lambda () (interactive) (forward-char 10)))

(general-define-key
 :keymaps 'ctl-x-map
 "`" 'open-terminal-here
 "w" 'kill-this-buffer
 "p" 'helm-projectile-find-file-in-known-projects
 "k" 'ido-kill-buffer
 "C-f" 'helm-find-files
 "C-g" 'config/reset-minibuffer
 "f" 'find-file
 "b" 'helm-buffers-list
 "t" 'new-empty-buffer
 "m" (general-simulate-key "C-c")
 "C-u" nil ; upcase-region
 "C-l" nil ; downcase-region
 "C-d" 'ido-dired
 "C-p" 'helm-projectile-find-file-in-known-projects
 "C-v" 'magit-status
 "C-b" nil
 "C-t" nil
 "C-j" 'hscroll-mode)

(general-define-key
 :prefix leader-key
 "p" projectile-command-map
 "C-p" projectile-command-map
 "b" 'ibuffer
 "C-d" 'desktop-save
 "C-a" 'config/treemacs-toggle-find-file-collapse-other-projects
 "C-b" 'config/treemacs-toggle-find-file
 "C-w" treemacs-project-map
 "C-f" 'helm-do-ag-project-root
 "C-g" 'config/reset-minibuffer
 "n" 'new-empty-buffer
 "v" 'magit-file-popup
 "C-v" 'magit-status)

(general-define-key
 :keymaps '(fundamental-mode-map text-mode-map special-mode-map)
 "C-d" (general-simulate-key "<next>")
 "C-u" (general-simulate-key "<prior>"))

(general-define-key
 :keymaps 'isearch-mode-map
 "C-f" 'isearch-repeat-forward
 "C-j" 'isearch-query-replace-regexp
 "<up>" 'isearch-ring-retreat
 "<down>" 'isearch-ring-advance)
