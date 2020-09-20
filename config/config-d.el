(use-package d-mode
  :straight t
  :defer t
  :mode ("\\.d\\'" . d-mode)
  :init
  (add-hook 'd-mode-hook
            (lambda ()
              (c-set-offset 'substatement-open 0)
              (c-set-offset 'case-label '+)
              (config/set-local-tab-width 2)
              (set (make-local-variable 'c-basic-offset) 2)
              (set (make-local-variable 'c-indent-level) 2))))

(provide 'config-d)
