;;; init.el -- Emacs configuration

;; Packages
(require 'package)
(setq package-list '(evil
		     helm
		     helm-projectile
		     helm-ag
		     spaceline
		     projectile
		     ido-vertical-mode
		     flx
		     flx-ido
		     tabbar
		     use-package
		     neotree
		     general
		     magit
		     company
		     web-mode
		     js2-mode
		     tide
		     evil-surround
		     window-numbering
		     expand-region
		     ensime
		     anzu
		     smooth-scroll
		     esup
		     clojure-mode
		     clojure-mode-extra-font-locking
		     cider
		     smartparens
		     iflipb))

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
(load "~/.emacs.d/local.el")
(dolist (local-setting '((default-dir . "~/")
			 (custom-font-face . "Inconsolata-12")
			 (desktop-window-attributes . '())
			 (client-window-attributes . '())))
  (when (not (boundp (car local-setting)))
    (set (car local-setting) (cdr local-setting))))

;; Startup options
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message () (message ""))

;; Window position
(setq configure-frame-functions '())
(if (daemonp)
  (setq default-frame-alist client-window-attributes)
  (progn
    (desktop-save-mode 1)
    (setq default-frame-alist desktop-window-attributes)))
(add-hook 'desktop-after-read-hook (lambda ()
  (cond ((string= (frame-parameter nil 'fullscreen) 'maximized)
	 (toggle-frame-maximized))
	((string= (frame-parameter nil 'fullscreen) 'fullboth)
	 (toggle-frame-fullscreen)
	 (toggle-frame-maximized)))))

;; Fix toggle-frame-fullscreen to preserve our window position
(defun force-maximized-with-fullscreen (orig-fun &rest args)
  (let ((fullscreen-parameter (frame-parameter nil 'fullscreen)))
    (unless (or (string= fullscreen-parameter 'fullboth)
		(string= fullscreen-parameter 'maximized))
      (toggle-frame-maximized)))
  (apply orig-fun args))
(advice-add 'toggle-frame-fullscreen :around #'force-maximized-with-fullscreen)

;; General config
(setq frame-title-format "Emacs - %b")
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-auto-revert-mode 1)
(blink-cursor-mode 0)
(setq-default cursor-in-non-selected-windows nil)
(setq tooltip-use-echo-area t)
(setq isearch-allow-scroll t)
(setq load-prefer-newer t)
(show-paren-mode 1)
;; (electric-pair-mode 1)
(global-superword-mode 1)

;; Line numbers
(global-linum-mode 1)
(setq linum-format "%4d ")
(setq resize-mini-windows t)

;; Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-error-top-bottom t)
(add-to-list 'configure-frame-functions
  (lambda ()
    (toggle-scroll-bar -1)
    (horizontal-scroll-bar-mode -1)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

;; Indentation
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq tab-stop-list (number-sequence 2 120 2))
(setq indent-line-function 'insert-tab)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Face attributes
(add-to-list 'default-frame-alist `(font . ,custom-font-face))
(set-face-attribute 'default nil :font custom-font-face)
(add-to-list 'configure-frame-functions
  (lambda ()
    (set-face-attribute 'vertical-border nil :foreground "#222222")
    (set-face-attribute 'ido-subdir nil :foreground "#90788c")))

;; Use UTF-8 throughout
(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
(set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

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
(global-set-key [C-backspace] 'backward-kill-word-fixed)

;; Evil
(setq evil-toggle-key "<f5>")
(require 'evil)
(evil-mode 1)
(setq evil-disable-insert-state-bindings t)
(evil-set-initial-state 'dired-mode 'normal)
(evil-set-initial-state 'Buffer-menu-mode 'normal)
; (with-eval-after-load 'evil
;   (defalias #'forward-evil-word #'forward-evil-symbol))

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
(setq helm-split-window-in-side-p t)
(setq helm-boring-buffer-regexp-list
  '("\\` " "\\Messages" "\\*scratch" "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))
(global-set-key (kbd "C-S-p") 'helm-M-x)
(global-set-key (kbd "C-p") 'helm-mini)

;; Projectile
(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-l p"))
  :config
  (setq projectile-indexing-method 'alien)
  ;; (setq projectile-enable-caching t)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".cache")
  (projectile-global-mode))

;; General
(require 'general)
(setq leader-key "C-l")
(general-define-key :prefix leader-key)

;; Magit
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))
(general-define-key
 :keymaps 'magit-mode-map
  "C-k" (general-simulate-keys "C-x")
  "SPC" (general-simulate-keys "M-x" t)
  "M-<f4>" (if (daemonp) 'delete-frame 'save-buffers-kill-emacs)
  "C-:" 'eval-expression
  "C-<tab>" 'previous-buffer
  "C-<prior>" 'tabbar-backward-tab
  "C-<next>" 'tabbar-forward-tab
  "C-u" 'evil-scroll-up
  "C-d" 'evil-scroll-down)

; Window Numbering
(defun window-numbering-install-mode-line (&optional position))
(window-numbering-mode 1)
(dolist (n (number-sequence 1 9))
  (global-set-key (kbd (format "C-%s" n)) (intern (format "select-window-%s" n))))

;; Anzu
(require 'anzu)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil :foreground "#639743" :weight 'normal)
(global-set-key [remap isearch-query-replace] 'anzu-isearch-query-replace)
(global-set-key [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)

;; Company
(require 'company)
(general-define-key
 :keymaps 'company-active-map
 "<tab>" (general-simulate-keys "<return>"))
(setq company-frontends
      '(company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))
(setq company-require-match nil)
(add-to-list 'configure-frame-functions
  (lambda ()
    (set-face-attribute 'company-tooltip nil :background "#1d2026")
    (set-face-attribute 'company-tooltip-annotation nil :foreground "#8787d7")
    (set-face-attribute 'company-tooltip-selection nil :foreground "#ffffff" :background "#4a4e54")
    (set-face-attribute 'company-tooltip-common-selection nil :foreground "#66a9d4")))
(add-hook 'emacs-lisp-mode-hook #'company-mode)

;; Spaceline
(require 'spaceline-config)
(setq powerline-default-separator nil)
(setq spaceline-workspace-numbers-unicode t)
(setq paceline-window-numbers-unicode t)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-off)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-anzu-off)
(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
(add-to-list 'configure-frame-functions
  (lambda ()
    (set-face-attribute 'spaceline-evil-insert nil :background "#7eaefd")
    (set-face-attribute 'spaceline-evil-normal nil :background "#4f3598" :foreground "#ffffff")
    (set-face-attribute 'spaceline-evil-replace nil :background "#005154" :foreground "#ffffff")
    (set-face-attribute 'spaceline-evil-visual nil :background "#e6987a")
    (set-face-attribute 'spaceline-evil-emacs nil :background "#393d44" :foreground "#ffffff")))
(spaceline-spacemacs-theme)
(spaceline-helm-mode 1)

;; Ido
(require 'flx-ido)
(require 'ido-vertical-mode)
(ido-mode 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(add-to-list 'configure-frame-functions
  (lambda ()
  (set-face-attribute 'ido-vertical-first-match-face nil :background nil :foreground "#5cacee")
  (set-face-attribute 'ido-vertical-only-match-face nil :background nil :foreground "#5cacee")
  (set-face-attribute 'ido-vertical-match-face nil :foreground "#5cacee")))

;; Tabbar
(setq tabbar-use-images nil)
(require 'tabbar)
(tabbar-mode 1)
(add-to-list 'configure-frame-functions
  (lambda ()
    (set-face-attribute 'tabbar-default nil :background "#202328" :foreground "#202328" :box '(:line-width 1 :color "#202328" :style nil) :font custom-font-face)
    (set-face-attribute 'tabbar-unselected nil :background "#202328" :foreground "#606060" :box '(:line-width 5 :color "#202328" :style nil))
    (set-face-attribute 'tabbar-selected nil :background "#272b33" :foreground "white" :box '(:line-width 5 :color "#272b33" :style nil))
    (set-face-attribute 'tabbar-modified nil :background "#202328" :foreground "#606060" :underline "#505050" :box '(:line-width 5 :color "#202328" :style nil))
    (set-face-attribute 'tabbar-selected-modified nil :background "#272b33" :foreground "white" :underline "#909090" :box '(:line-width 5 :color "#272b33" :style nil))
    (set-face-attribute 'tabbar-highlight nil :background "white" :foreground "black" :underline nil :box '(:line-width 5 :color "white" :style nil))
    (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "#202328" :style nil))
    (set-face-attribute 'tabbar-separator nil :background "#202328" :height 0.6)
    (tabbar-forward-tab) ; Force redraw to fix colors
    (tabbar-backward-tab)))
(global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)
(global-set-key (kbd "C-<next>") 'tabbar-forward-tab)

;; Tabbar visual tweaks
;; https://gist.github.com/3demax/1264635
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

;; Tabbar-Ruler projectile groups
;; https://github.com/mattfidler/tabbar-ruler.el
(defvar tabbar-projectile-tabbar-buffer-group-calc nil)
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
            (neotree-dir default-dir)
          (let ((dir-name (if (and (fboundp 'projectile-project-p)
                                   (projectile-project-p))
                              (projectile-project-root)
                            (file-name-directory file-name))))
            (neotree-dir dir-name)
            (when file-name
              (neo-buffer--select-file-node file-name)))))))

;; Smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

;; Smooth-Scoll
(use-package smooth-scroll
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 5))

;; Expand-Region
(require 'expand-region)

;; Yasnippets
(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

;; HTML / CSS / JavaScript
(require 'web-mode)
(dolist (assoc '(("\\.phtml\\'"     . web-mode)
	         ("\\.tpl\\.php\\'" . web-mode)
	         ("\\.[agj]sp\\'"   . web-mode)
	         ("\\.as[cp]x\\'"   . web-mode)
	         ("\\.erb\\'"       . web-mode)
	         ("\\.mustache\\'"  . web-mode)
	         ("\\.djhtml\\'"    . web-mode)
	         ("\\.html?\\'"     . web-mode)
	         ("\\.js\\'"        . js2-mode)
	         ("\\.jsx?\\'"      . js2-jsx-mode)
		 ("node"            . js2-jsx-mode)))
  (add-to-list 'auto-mode-alist assoc))
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
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(idle-change mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (company-mode 1))
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

;; Scala
(use-package ensime
  :ensure t)

;; Clojure
(require 'clojure-mode)
(setq clojure-indent-style :always-indent)
(add-hook 'clojure-mode-hook #'company-mode)
(add-hook 'clojure-mode-hook (lambda () (setq evil-shift-width 2)))
(define-clojure-indent
  (match 1)
  (are 2)
  (checking 2)
  (async 1))
(define-clojure-indent
  (go-loop 1))
(define-clojure-indent
  (this-as 1)
  (specify 1)
  (specify! 1))

;; Cider
(require 'cider)
(setq cider-repl-history-file "~/.emacs.d/cider-history")
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-use-pretty-printing t)
(setq cider-repl-use-clojure-font-lock t)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-result-prefix ";; => ")
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 3000)
(setq cider-use-fringe-indicators nil)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-show-error-buffer nil)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-to-list 'evil-emacs-state-modes 'cider-repl-mode)
(setq cider-cljs-lein-repl
  "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
(general-define-key
 :keymaps 'cider-mode-map
 "C-c C-n" 'cider-repl-set-ns)
(general-define-key
 :keymaps 'cider-repl-mode-map
 "C-c C-l" 'cider-repl-clear-buffer)

;; Popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)
(setq helm-split-window-preferred-function 'ignore)

;; Iflipb
(require 'iflipb)
(global-set-key (kbd "C-<tab>") 'iflipb-next-buffer)
(global-set-key (kbd "<C-iso-lefttab>") 'iflipb-previous-buffer)
(setq iflipb-wrap-around t)

;; Load theme
(setq custom-theme-directory "~/.emacs.d/themes/")
(add-to-list 'configure-frame-functions (lambda () (load-theme 'custom-dark t)))

;; Fix fonts and other stuff in clients
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

;; Keybindings
(global-set-key (kbd "C-k") ctl-x-map)

(dolist
  (key '("M-<DEL>" "M-u" "M-i" "M-o" "M-p" "M-k" "M-l" "M-m" "M-:" "M-/"))
  (global-set-key (kbd key) nil))

(defun unset-key () (interactive) ())
(general-define-key
  "C-:" 'eval-expression
  "M-<f4>" (if (daemonp) 'delete-frame 'save-buffers-kill-emacs)
  "C-=" 'enlarge-window-horizontally
  "C--" 'shrink-window-horizontally
  "C-_" 'enlarge-window
  "C-+" 'shrink-window
  "S-<left>" 'windmove-left
  "S-<right>" 'windmove-right
  "S-<up>" 'windmove-up
  "S-<down>" 'windmove-down
  "M-<up>" (lambda () (interactive) (previous-line 10))
  "M-<down>" (lambda () (interactive) (next-line 10)))
(general-define-key
  :states '(normal)
  "q" 'unset-key
  "r" 'unset-key)
(general-define-key
 :states '(insert)
 "<tab>" 'tab-to-tab-stop
 "C-c" 'kill-ring-save
 "C-x" 'kill-region
 "C-v" 'yank)
(general-define-key
  :states '(visual)
  "S-<left>" (lambda () (interactive) (backward-char))
  "S-<right>" (lambda () (interactive) (forward-char))
  "S-<up>" (lambda () (interactive) (previous-line))
  "S-<down>" (lambda () (interactive) (next-line)))
(general-define-key
 :states '(normal visual)
 "SPC" (general-simulate-keys "M-x" t)
 "C-v" 'er/expand-region)
(general-define-key
 :states '(normal insert visual)
 "C-z" 'undo-tree-undo
 "C-s" 'save-buffer
 "C-f" 'isearch-forward-regexp
 "C-S-f" 'isearch-backward-regexp
 "C-h" 'query-replace-regexp
 "C-S-v" 'evil-visual-block
 "C-b" 'unset-key)
(general-define-key
 :states '(normal insert visual motion)
 "C-u" 'evil-scroll-up
 "C-q" (lambda () (interactive) (scroll-down 1)))
(general-define-key
  :states '(normal insert visual emacs motion)
  "C-/" 'comment-region
  "<home>" 'back-to-indentation
  "C-_" 'enlarge-window
  "C-S-p" 'helm-M-x
  "C-p" 'helm-projectile-find-file-dwim)
(general-define-key
  :keymaps 'ctl-x-map
  "w" 'kill-this-buffer
  "b" 'helm-mini
  "k" 'ido-kill-buffer
  "C-b" 'neotree-projectile)
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
    (smartparens cider clojure-mode-extra-font-locking clojure-mode esup smooth-scroll anzu ensime expand-region window-numbering guide-key evil-surround tide web-mode use-package typescript-mode tabbar spaceline neotree magit js2-mode ido-vertical-mode helm-projectile helm-ag general flycheck flx-ido evil company))))
