;;; config.el --- eaf-extra configuration File for Spacemacs
;; Time-stamp: <2023-04-12 Wed 14:37 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

(spacemacs|use-package-add-hook eaf
  :pre-init
  (setq eaf-apps-to-install
        '(browser
          pdf-viewer
          ;; music-player
          ;; video-player
          image-viewer
          ;; system-monitor
          terminal
          ;; git
          ;; map
          rss-reader
          mindmap
          markmap
          markdown-previewer
          org-previewer
          ;;file-manager ;;FIXME: failed to install
          ;;file-browser
          ))
  (setq eaf-apps
        '(browser
          pdf-viewer
          ;; music-player
          ;; video-player
          image-viewer
          ;; system-monitor
          terminal
          ;; git
          ;; map
          rss-reader
          mindmap
          markmap
          markdown-previewer
          org-previewer
          ;;file-manager ;;FIXME: failed to install
          ;;file-browser
          ))
  :post-config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  ;; (require 'eaf-music-player)
  ;; (require 'eaf-video-player)
  (require 'eaf-image-viewer)
  ;; (require 'eaf-system-monitor)
  (require 'eaf-terminal)
  ;; (require 'eaf-git)
  ;; (require 'eaf-map)
  (require 'eaf-rss-reader)
  (require 'eaf-mindmap)
  (require 'eaf-markmap)
  (require 'eaf-markdown-previewer)
  (require 'eaf-org-previewer)
  ;; (require 'eaf-file-manager) 
  ;; (require 'eaf-file-browser)
  (require 'eaf-org)
  ;;-------------------------------------
  ;; eaf-terminal
  (setq eaf-terminal-font-family "FiraCode Nerd Font"
        eaf-terminal-font-size 16)
  ;; TODO: hide mode-line
  ;; (advice-add 'eaf-open-terminal :filter-return #'spacemacs/toggle-mode-line-off)
  ;; (advice-add 'eaf-browser-open :filter-return #'spacemacs/toggle-mode-line-off)

  ;; my tmux prefix M-z
  (add-to-list 'eaf-terminal-keybinding '("M-z" . "eaf-send-key-sequence"))
  ;; fzf
  (add-to-list 'eaf-terminal-keybinding '("C-r" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("C-t" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-c" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-t" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-e" . "eaf-send-key-sequence"))
  ;; mc
  (add-to-list 'eaf-terminal-keybinding '("C-\\" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-i" .  "eaf-send-key-sequence"))
  ;; zellij
  (add-to-list 'eaf-terminal-keybinding '("C-g" . "eaf-send-key-sequence"))
  ;; (add-to-list 'eaf-terminal-keybinding '("C-t" . "eaf-send-key-sequence")) ;; duplicated
  (add-to-list 'eaf-terminal-keybinding '("C-s" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("C-h" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("C-q" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-n" . "eaf-send-key-sequence"))
  ;; (add-to-list 'eaf-terminal-keybinding '("M-<up>" . "eaf-send-key-sequence"))
  ;; (add-to-list 'eaf-terminal-keybinding '("M-<down>" . "eaf-send-key-sequence"))
  ;; (add-to-list 'eaf-terminal-keybinding '("M-<left>" . "eaf-send-key-sequence"))
  ;; (add-to-list 'eaf-terminal-keybinding '("M-<right>" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-h" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-j" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-k" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding '("M-l" . "eaf-send-key-sequence"))
  ;; (add-to-list 'eaf-terminal-keybinding '("M-+" . "eaf-send-key-sequence"))
  ;; (add-to-list 'eaf-terminal-keybinding '("M--" . "eaf-send-key-sequence"))
  ;;---------------------------------------

  ;; eaf-browser
  (setq eaf-browser-default-search-engine "google" ;; 设定eaf默认搜索引擎
        eaf-browser-enable-adblocker t   ;; 设定eaf开启广告屏蔽器
        eaf-browser-default-zoom 1.2 ;; 设定eaf浏览器的缩放
        eaf-webengine-download-path "~/下载/eaf-browser"
        eaf-proxy-type "socks5"
        eaf-proxy-host "127.0.0.1"
        eaf-proxy-port "7890")
  (add-to-list 'eaf-browser-keybinding '("C" . "xy/open-current-webpage-in-chrome"))
  (add-to-list 'eaf-browser-keybinding '("w" . "eaf-get-path-or-url"))
  (add-to-list 'eaf-browser-keybinding '("C-c C-l" . "eaf-org-store-link"))
  (add-to-list 'eaf-browser-keybinding '("C-m" . "eaf-send-return-key"))
  (setq eaf-browser-keybinding (delete '("M-m" . "eaf-send-return-key") eaf-browser-keybinding))

  (xy/set-eaf-browser-as-default)
  )
