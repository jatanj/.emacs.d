(setq c-basic-offset 4)
(setq c-indent-level 4)
(setq-default c-default-style "k&r")
(add-hook 'c-mode-common-hook
          (lambda ()
            (customize-cc-mode)
            (rainbow-delimiters-mode-enable)))

(defun customize-cc-mode ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (set-local-tab-width 4))

(provide 'config-c-cpp)
