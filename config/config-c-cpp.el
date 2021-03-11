(setq c-basic-offset 4)
(setq c-indent-level 4)
(setq-default c-default-style '((java-mode . "java")
                                (other . "bsd")))

(defun config/c-mode-init-common ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'innamespace 0)
  (config/set-local-tab-width 4))
(add-hook 'c-mode-common-hook #'config/c-mode-init-common)

(defun config/c++-mode-init ()
  (setq-local fill-column 120)
  ;; (fci-mode 1)
  (yas-minor-mode 1))
(add-hook 'c++-mode-hook #'config/c++-mode-init)

(defun config/cpp-file-template ()
  (let* ((path (buffer-file-name))
         (ext (file-name-extension path)))
    (when (= (point-min) (point-max))
      (cond ((member ext '("h" "hpp"))
             (insert "#pragma once")
             (newline 2))
            ((member ext '("cc" "cpp", "cxx", "C"))
             (let* ((file-name (file-name-sans-extension (file-name-nondirectory path)))
                    (files (directory-files (file-name-directory (buffer-file-name)))))
               (when-let ((ext (car (--filter (member (concat file-name it) files) '(".h" ".hpp")))))
                 (insert (format "#include \"%s%s\"" file-name ext))
                 (newline 2))))))))
(add-hook 'find-file-hook #'config/cpp-file-template)

(use-package cmake-mode
  :straight t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)))

(provide 'config-c-cpp)
