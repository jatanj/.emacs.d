(load "~/.emacs.d/local.el")

;; Startup options
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message () (message ""))
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Load theme
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'custom-dark t)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'vertical-border nil :foreground "#202020")

;; Indentation
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq tab-stop-list (number-sequence 2 120 2))
(setq indent-line-function 'insert-tab)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Use UTF-8 throughout
(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
(set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; General config
(desktop-save-mode 1)
(setq frame-title-format "Emacs - %b")
(show-paren-mode 1)
(global-linum-mode 1)
(setq linum-format " %d ")
(blink-cursor-mode 0)
(setq resize-mini-windows t)
(toggle-scroll-bar -1)
(setq scroll-step 1)
(setq scroll-error-top-bottom t)
(setq tooltip-use-echo-area t)

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

(set-face-attribute 'default nil :font global-font-face)
(custom-set-faces
 '(ido-subdir ((t (:foreground "#8b7d6b")))))

;; Packages
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(custom-set-variables
 '(custom-safe-themes
   (quote
    ("7968290e621e86fb44ebfcaa4d17601087ae17b28dd689ddff467179c2983164" "f080d47fc227ba4d7129df8ff5b2aaa9ec50ea242cff220dc3758b3fadd3ef78" "fcaa761fedb6bacfc7b0c0551d3b710568d0da4eb3124bf86f7c6bedf3170296" default)))
 '(package-selected-packages
   (quote
    (js2-mode helm-ag ag helm-projectile general neotree use-package flx-ido tabbar ido-vertical-mode projectile spaceline helm evil))))

;; Javascript / HTML / CSS
(setq js-indent-level 2)
(setq javascript-indent-level 2)
(setq js2-basic-offset 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)

;; Evil
(setq evil-toggle-key "C-`")
(require 'evil)
(evil-mode 1)
(setq evil-disable-insert-state-bindings t)
(evil-set-initial-state 'dired-mode 'normal)
(evil-set-initial-state 'Buffer-menu-mode 'normal)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; Helm
(require 'helm)
(require 'helm-config)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 25)
(setq helm-buffers-fuzzy-matching t)

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-keymap-prefix (kbd "C-l p"))
  :config
  (projectile-global-mode)
  (setq projectile-use-native-indexing t)
  (setq projectile-enable-caching t))

;; Spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)
(setq powerline-default-separator 'arrow-fade)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-off)
(spaceline-toggle-buffer-size-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(set-face-attribute 'spaceline-evil-insert nil :background "#7eaefd")
(set-face-attribute 'spaceline-evil-normal nil :background "#4f3598" :foreground "#ffffff")
(set-face-attribute 'spaceline-evil-replace nil :background "#005154" :foreground "#ffffff")
(set-face-attribute 'spaceline-evil-visual nil :background "#e6987a")

;; Ido
(require 'flx-ido)
(require 'ido-vertical-mode)
(ido-mode 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(set-face-attribute 'ido-vertical-first-match-face nil :background nil :foreground "#5cacee")
(set-face-attribute 'ido-vertical-only-match-face nil :background nil :foreground "#5cacee")
(set-face-attribute 'ido-vertical-match-face nil :foreground "#5cacee")

;; Tabbar
(setq tabbar-use-images nil)
(require 'tabbar)
(tabbar-mode 1)
(set-face-attribute 'tabbar-default nil :background "#202328" :foreground "#202328" :box '(:line-width 1 :color "#202328" :style nil) :font global-font-face)
(set-face-attribute 'tabbar-unselected nil :background "#202328" :foreground "#606060" :box '(:line-width 5 :color "#202328" :style nil))
(set-face-attribute 'tabbar-selected nil :background "#272b33" :foreground "white" :box '(:line-width 5 :color "#272b33" :style nil))
(set-face-attribute 'tabbar-highlight nil :background "white" :foreground "black" :underline nil :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "#202328" :style nil))
(set-face-attribute 'tabbar-separator nil :background "#202328" :height 0.6)
;; Change padding of the tabs
;; We also need to set separator to avoid overlapping tabs by highlighted tabs
;; Adding spaces
(setq tabbar-separator (quote (0.5)))
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB. That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

;; NeoTree
(require 'neotree)
(setq neo-theme 'ascii)
(defun neotree-projectile ()
  "Open neotree with projectile as root and open node for current file.
  If projectile unavailable or not in a project, open node at file path.
  If file path is not available, open $HOME."
    (interactive)
    (if (neo-global--window-exists-p)
        (call-interactively 'neotree-hide)
      (let ((file-name (buffer-file-name)))
        (if (and (not file-name)
                 (let ((buffer-name (buffer-name)))
                   (cond
                    ((equal buffer-name "*cider-repl server*") nil)
                    (t t))))
            (neotree-dir develop-dir)
          (let ((dir-name (if (and (fboundp 'projectile-project-p)
                                   (projectile-project-p))
                              (projectile-project-root)
                            (file-name-directory file-name))))
            (neotree-dir dir-name)
            (when file-name
              (neo-buffer--select-file-node file-name)))))))

(require 'general)

(dolist
  (key '("M-u" "M-i" "M-o" "M-p" "M-k" "M-l" "M-m" "M-/"))
  (global-set-key (kbd key) nil))

(global-set-key (kbd "C-k") ctl-x-map)
(global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)
(global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
(global-set-key (kbd "C-r") (general-simulate-keys "C-M-%" t))

(define-key global-map (kbd "C-h") nil)
(global-unset-key (kbd "C-h"))
(setq help-char nil)

(general-define-key
  :states '(normal emacs motion)
  "SPC" (general-simulate-keys "M-x" t))

(general-define-key
  :states '(insert)
  "TAB" 'tab-to-tab-stop)

(general-define-key
  :states '(normal insert visual emacs motion)
  "M-<f4>" 'save-buffers-kill-emacs
  "C-z" 'undo-tree-undo
  "C-s" 'save-buffer
  "C-f" 'isearch-forward-regexp
  "C-S-f" 'isearch-backward-regexp
  "C-x" 'kill-region
  "C-c" 'kill-ring-save
  "C-v" 'yank
  "<C-tab>" 'mode-line-other-buffer
  "M-C-<left>" 'windmove-left
  "M-C-<right>" 'windmove-right
  "M-C-<up>" 'windmove-up
  "M-C-<down>" 'windmove-down
  "M-<up>" (lambda () (interactive) (previous-line 10))
  "M-<down>" (lambda () (interactive) (next-line 10))
  "C-S-p" 'helm-M-x
  "C-p" 'helm-mini)

(setq leader-key "C-l")
(general-define-key :prefix leader-key)

(general-define-key
  :keymaps 'ctl-x-map
  "C-b" 'neotree-projectile)
