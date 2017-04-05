;;; evening-dark-theme.el
;;; Inspired by Base2Tone Evening Dark, http://base2t.one.

(require 'custom-theme-common)

(deftheme evening-dark)
(custom-theme-set-common
 'evening-dark
 ((class '((class color) (min-colors 89)))
   (fg1       "#f8f8f8")
   (fg2       "#e4e4e4")
   (fg3       "#d0d0d0")
   (fg4       "#bcbcbc")
   (fg5       "#ffffff")
   (fg6       "#000000")
   (bg1       "#2a2734")
   (bg2       "#363342")
   (bg3       "#545167")
   (bg4       "#6c6783")
   (bg5       "#7a768f")
   (bg6       "#1e1c25")
   (red       "#bf616a")
   (green     "#a3be8c")
   (yellow    "#ebcb8b")
   (blue      "#81a1c1")
   (magenta   "#8989c4")
   (cyan      "#5aa4c2")
   (builtin   "#90788c")
   (keyword   "#c3a287")
   (const     fg1)
   (func      fg1)
   (str       "#ffebb5")
   (type      "#8787d7")
   (var       fg1)
   (tag       "#4fc1e9")
   (border    "#262330")
   (match     "#434052")
   (success   "#499fbc")
   (success2  "#387c98")
   (info      "#639743")
   (error     "#bf6a6a")
   (error2    "#663333")
   (warning   "#ffff00")))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'evening-dark)
