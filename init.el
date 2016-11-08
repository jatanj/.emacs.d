(load "~/.emacs.d/local.el")

;; Startup options
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message () (message ""))

(setq configure-frame-functions '())

;; General config
(if (daemonp)
  (setq default-frame-alist client-window-attributes)
  (progn
    (desktop-save-mode 1)
    (setq default-frame-alist desktop-window-attributes)))
(setq frame-title-format "Emacs - %b")
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(global-linum-mode 1)
(setq linum-format " %d ")
(setq resize-mini-windows t)
(setq scroll-step 1)
(setq scroll-error-top-bottom t)
(setq tooltip-use-echo-area t)
(add-to-list 'configure-frame-functions '(lambda () (toggle-scroll-bar -1)))
(global-superword-mode 1)
(electric-pair-mode 1)

;; Indentation
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq tab-stop-list (number-sequence 2 120 2))
(setq indent-line-function 'insert-tab)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Face attributes
(add-to-list 'default-frame-alist `(font . ,global-font-face))
(set-face-attribute 'default nil :font global-font-face)
(add-to-list 'configure-frame-functions
  '(lambda ()
    (set-face-attribute 'vertical-border nil :foreground "#222222")
    (set-face-attribute 'ido-subdir nil :foreground "#90788c")
    (set-face-attribute 'tabbar-default nil :background "#202328" :foreground "#202328" :box '(:line-width 1 :color "#202328" :style nil) :font global-font-face)
    (set-face-attribute 'tabbar-unselected nil :background "#202328" :foreground "#606060" :box '(:line-width 5 :color "#202328" :style nil))
    (set-face-attribute 'tabbar-selected nil :background "#272b33" :foreground "white" :box '(:line-width 5 :color "#272b33" :style nil))
    (set-face-attribute 'tabbar-highlight nil :background "white" :foreground "black" :underline nil :box '(:line-width 5 :color "white" :style nil))
    (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "#202328" :style nil))
    (set-face-attribute 'tabbar-separator nil :background "#202328" :height 0.6)))

;; Use UTF-8 throughout
(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
(set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; Copy/cut entire line if no region is selected
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

; ;; Get rid of buffer messages
; (defun my-ignore-bet-end-of-buffer--around-ad (f &rest args)
;   (condition-case nil
;       (apply f args)
;     ((beginning-of-buffer end-of-buffer))))

; (advice-add 'left-char :around
;             #'my-ignore-bet-end-of-buffer--around-ad)

; (advice-add 'right-char :around
;             #'my-ignore-bet-end-of-buffer--around-ad)

;; Packages
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7968290e621e86fb44ebfcaa4d17601087ae17b28dd689ddff467179c2983164" "f080d47fc227ba4d7129df8ff5b2aaa9ec50ea242cff220dc3758b3fadd3ef78" "fcaa761fedb6bacfc7b0c0551d3b710568d0da4eb3124bf86f7c6bedf3170296" default)))
 '(package-selected-packages
   (quote
    (window-numbering guide-key evil-surround web-mode tide company flycheck js2-mode helm-ag ag helm-projectile general neotree use-package flx-ido tabbar ido-vertical-mode projectile spaceline helm evil))))

;; Evil Mode
(setq evil-toggle-key "C-S-`")
(require 'evil)
(evil-mode 1)
(setq evil-disable-insert-state-bindings t)
(evil-set-initial-state 'dired-mode 'normal)
(evil-set-initial-state 'Buffer-menu-mode 'normal)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; Evil Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Helm
(require 'helm)
(require 'helm-config)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 25)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
; (add-to-list 'configure-frame-functions '(lambda ()
;   (set-face-attribute 'helm-match nil :foreground "#5cacee" :underline nil)))

;; Projectile
(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-l p"))
  :config
  (setq projectile-use-native-indexing t)
  (setq projectile-enable-caching t)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-global-mode))

; Window Numbering
(defun window-numbering-install-mode-line (&optional position))
(window-numbering-mode 1)
(dolist (n (number-sequence 1 9))
  (global-set-key (kbd (format "C-%s" n)) (intern (format "select-window-%s" n))))

;; Spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)
(setq powerline-default-separator nil)
(setq spaceline-workspace-numbers-unicode t)
(setq paceline-window-numbers-unicode t)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-off)
(spaceline-toggle-buffer-size-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(add-to-list 'configure-frame-functions '(lambda ()
  (set-face-attribute 'spaceline-evil-insert nil :background "#7eaefd")
  (set-face-attribute 'spaceline-evil-normal nil :background "#4f3598" :foreground "#ffffff")
  (set-face-attribute 'spaceline-evil-replace nil :background "#005154" :foreground "#ffffff")
  (set-face-attribute 'spaceline-evil-visual nil :background "#e6987a")))
(spaceline-compile)

;; Ido
(require 'flx-ido)
(require 'ido-vertical-mode)
(ido-mode 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(add-to-list 'configure-frame-functions '(lambda ()
  (set-face-attribute 'ido-vertical-first-match-face nil :background nil :foreground "#5cacee")
  (set-face-attribute 'ido-vertical-only-match-face nil :background nil :foreground "#5cacee")
  (set-face-attribute 'ido-vertical-match-face nil :foreground "#5cacee")))

;; Tabbar
(setq tabbar-use-images nil)
(require 'tabbar)
(tabbar-mode 1)
; https://gist.github.com/3demax/1264635
(setq tabbar-separator (quote (0.5)))
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB. That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))
; Tabbar Ruler projectile groups
(defvar tabbar-projectile-tabbar-buffer-group-calc nil
  "Buffer group for projectile.  Should be buffer local and speed up calculation of buffer groups.")
(defun tabbar-projectile-tabbar-buffer-groups ()
  "Return the list of group names BUFFER belongs to.
    Return only one group for each buffer."
  (if tabbar-projectile-tabbar-buffer-group-calc
      (symbol-value 'tabbar-projectile-tabbar-buffer-group-calc)
    (set (make-local-variable 'tabbar-projectile-tabbar-buffer-group-calc)
         (cond
          ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
          ((string-equal "*" (substring (buffer-name) 0 1)) '("Emacs"))
          ((condition-case err
               (projectile-project-root)
             (error nil)) (list (projectile-project-name)))
          ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
          ((memq major-mode '(dired-mode)) '("Dir"))
          (t '("User"))))
    (symbol-value 'tabbar-projectile-tabbar-buffer-group-calc)))
(setq tabbar-buffer-groups-function 'tabbar-projectile-tabbar-buffer-groups)

; (tabbar-group-by-projectile-project)
; (defun emacs-user-tabbar-groups ()
;    (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
;                ((eq major-mode 'dired-mode) "emacs")
;                (t "user"))))

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

;; Guide Key
; (require 'guide-key)
; (guide-key-mode 1)
; (setq guide-key/guide-key-sequence '("C-l p"))

;; HTML / CSS /3 JavaScript
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
(setq js-indent-level 2)
(setq javascript-indent-level 2)
(setq js2-basic-offset 2)
(setq js3-indent-level 2)
(setq sgml-basic-offset 2)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)

;; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(idle-change mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq typescript-indent-level 2)
(setq tide-format-options
  '(:indentSize 2
    :tabSize 2
    :convertTabsToSpaces t))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; Load theme
(setq custom-theme-directory "~/.emacs.d/themes/")
(add-to-list 'configure-frame-functions (lambda () (load-theme 'custom-dark t)))

(defun configure-frame ()
  (dolist (func configure-frame-functions)
    (funcall func)))
(if (daemonp)
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (select-frame frame)
      (configure-frame)))
  (configure-frame))

(dolist
  (key '("M-u" "M-i" "M-o" "M-p" "M-k" "M-l" "M-m" "M-/"))
  (global-set-key (kbd key) nil))

(global-set-key (kbd "C-k") ctl-x-map)
(global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)
(global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
(global-set-key (kbd "M-C-<prior>") 'enlarge-window)
(global-set-key (kbd "M-C-<next>") 'shrink-window)
(global-set-key (kbd "M-C-<home>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-C-<end>") 'shrink-window-horizontally)
(global-set-key (kbd "C-r") (general-simulate-keys "C-M-%" t))

(define-key global-map (kbd "C-h") nil)
(global-unset-key (kbd "C-h"))
(setq help-char nil)

(require 'general)
(setq leader-key "C-l")
(general-define-key :prefix leader-key)
(general-define-key
  :states '(normal)
  "S-<left>" (lambda () (interactive) (evil-visual-char) (backward-char))
  "S-<right>" (lambda () (interactive) (evil-visual-char) (forward-char))
  "S-<up>" (lambda () (interactive) (evil-visual-char) (previous-line))
  "S-<down>" (lambda () (interactive) (evil-visual-char) (next-line)))
(general-define-key
  :states '(visual)
  "S-<left>" (lambda () (interactive) (backward-char))
  "S-<right>" (lambda () (interactive) (forward-char))
  "S-<up>" (lambda () (interactive) (previous-line))
  "S-<down>" (lambda () (interactive) (next-line)))
(general-define-key
  :states '(normal emacs motion)
  "SPC" (general-simulate-keys "M-x" t))
(general-define-key
  :states '(insert)
  "TAB" 'tab-to-tab-stop)
(general-define-key
  :states '(normal insert visual emacs motion)
  "M-<f4>" 'save-buffers-kill-emacs
  "C-w" 'evil-normal-state
  "C-z" 'undo-tree-undo
  "C-s" 'save-buffer
  "C-f" 'isearch-forward-regexp
  "C-S-f" 'isearch-backward-regexp
  "C-x" 'kill-region
  "C-v" 'yank
  "C-S-v" 'evil-visual-block
  "C-q" (lambda () (interactive) (scroll-down 1))
  "<home>" 'back-to-indentation
  "<C-tab>" 'mode-line-other-buffer
  "M-C-<left>" 'windmove-left
  "M-C-<right>" 'windmove-right
  "M-C-<up>" 'windmove-up
  "M-C-<down>" 'windmove-down
  "M-<up>" (lambda () (interactive) (previous-line 10))
  "M-<down>" (lambda () (interactive) (next-line 10))
  "C-S-p" 'helm-M-x
  "C-p" 'helm-mini)
(general-define-key
  :keymaps 'ctl-x-map
  "w" 'kill-this-buffer
  "C-b" 'neotree-projectile)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
