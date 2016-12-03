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

(provide 'config-ido)
