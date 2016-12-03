(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)

(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

(setq helm-split-window-preferred-function 'ignore)

(provide 'config-popwin)
