;; nord-theme.el
;;; Inspired by the Nord color palette, https://github.com/arcticicestudio/nord.

(require 'custom-theme-common)

(deftheme nord)
(custom-theme-set-common
 'nord
 ((class '((class color) (min-colors 89)))
  (fg1 "#f8f8f8")
  (fg2 "#e4e4e4")
  (fg3 "#d0d0d0")
  (fg4 "#bcbcbc")
  (fg5 "#ffffff")
  (fg6 "#000000")
  (bg1 "#2c303d")
  (bg2 "#3b4252")
  (bg3 "#434c5e")
  (bg4 "#4c566a")
  (bg5 "#5d6678")
  (bg6 "#242839")
  (bg7 "#747f94")
  (red       "#bf616a")
  (green     "#a3be8c")
  (yellow    "#ebcb8b")
  (blue      "#81a1c1")
  (magenta   "#8989c4")
  (cyan      "#5aa4c2")
  (builtin   "#90788c")
  (keyword   "#90788c")
  (const     fg1)
  (func      fg1)
  (str       "#ffebb5")
  (type      "#8787d7")
  (var       fg1)
  (tag       "#4fc1e9")
  (border    "#272b36")
  (match     "#484b5a")
  (success   "#499fbc")
  (success2  "#387c98")
  (info      "#639743")
  (error     "#bf6a6a")
  (error2    "#663333")
  (warning   "#ffff00")
  (delim     "#89ddff")
  (ml-border "#242839")))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nord)
