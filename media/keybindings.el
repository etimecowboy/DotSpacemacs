(spacemacs/declare-prefix "am" "media")

(spacemacs/set-leader-keys
  "ame" 'spacemacs/emms-transient-state/body)

(global-set-key (kbd "C-<f11>") 'spacemacs/emms-transient-state/body)
;; (global-set-key (kbd "<f6>") 'spacemacs/mpvi-transient-state/body)
;; (global-set-key (kbd "C-<f6>") 'spacemacs/mpvi-seek-transient-state/body)

(spacemacs/set-leader-keys-for-major-mode 'emms-browser-mode
  "g" 'emms-playlist-mode-go)

(spacemacs/set-leader-keys
  "amm" 'mpvi-emms-add
  "amo" 'mpvi-open
  "amf" 'mpvi-open-from-favors
  "ami" 'mpvi-insert
  "ams" 'mpvi-seek
  "amT" 'mpvi-current-playing-load-subtitle
  "amG" 'emms-playlist-mode-go-popup
  "amg" 'emms-playlist-mode-go
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
