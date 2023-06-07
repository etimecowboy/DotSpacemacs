(spacemacs/declare-prefix "am" "media")
(spacemacs/declare-prefix "ame" "emms")

(spacemacs/set-leader-keys
  "ameb" 'emms-browser
  "amel" 'emms-playlist-mode-go-popup
  "amef" 'emms-play-file
  "amed" 'emms-play-directory
  "amep" 'emms-pause
  "amen" 'emms-next
  "amep" 'emms-previous
  "ames" 'emms-stop
  "amer" 'emms-shuffle
  "ameR" 'emms-random
  "ameS" 'emms-show
  "ameM" 'emms-display-modes
  "ame+" 'emms-volume-raise
  "ame-" 'emms-volume-lower
  "ame>" 'emms-seek-forward
  "ame<" 'emms-seek-backward
  "ame=" 'emms-seek-to)

(spacemacs/set-leader-keys-for-major-mode 'emms-browser-mode
  "g" 'emms-playlist-mode-go)

(spacemacs/set-leader-keys
  "amm" 'mpvi-emms-integrated-mode
  "amo" 'mpvi-open
  "amf" 'mpvi-open-from-favors
  "ams" 'mpvi-seek
  "amb" 'bilibili-fav-it
  "amt" 'bilibili-triple-it)

(spacemacs/declare-prefix-for-mode 'org-mode "B" "Bilibili")
(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "Br" 'bilibili-insert-recommend
  "Bf" 'bilibili-insert-favs
  "Bk" 'bilibili-insert-ranking
  "Bp" 'bilibili-insert-popular
  "Be" 'bilibili-insert-precious
  "Bu" 'bilibili-insert-upper-videos
  "BU" 'bilibili-insert-upper-season-videos
  "Bs" 'bilibili-insert-search
)
