(require 'guide-key)
(setq guide-key/guide-key-sequence t)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*guide-key*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.40)))

(setq guide-key/popup-window-position 'bottom)

(provide 'config-guide-key)
