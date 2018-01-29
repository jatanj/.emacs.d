;;; evening-light-theme.el
;;; Inspired by Base2Tone Evening Light, http://base2t.one.

(require 'custom-theme-common)

(deftheme evening-light)
(custom-theme-set-common
 'evening-light
 ((class '((class color) (min-colors 89)))
   (fg1       "#202020")
   (fg2       "#2a2734")
   (fg3       "#363342")
   (fg4       "#545167")
   (fg5       "#6c6783")
   (fg6       "#1e1c25")
   (bg1       "#eaebed")
   (bg2       "#cfcfdb")
   (bg3       "#a5a5af")
   (bg4       "#94949d")
   (bg5       "#85858d")
   (bg6       "#cfcfdb")
   (red       "#bf616a")
   (green     "#a3be8c")
   (yellow    "#ebcb8b")
   (blue      "#81a1c1")
   (magenta   "#8989c4")
   (cyan      "#5aa4c2")
   (builtin   "#7a6554")
   (keyword   "#493c32")
   (const     fg1)
   (func      fg1)
   (str       "#a89164")
   (type      "#8787d7")
   (var       fg1)
   (tag       "#4fc1e9")
   (border    "#d7d7e1")
   (match     "#d2d2ff")
   (success   "#499fbc")
   (success2  "#81c3d1")
   (info      "#639743")
   (error     "#bf6a6a")
   (error2    "#663333")
   (warning   "#ffff00")))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'evening-light)
