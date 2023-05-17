;;; packages.el --- eaf-extra layer packages File for Spacemacs
;; Time-stamp: <2023-05-17 Wed 09:09 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;
;;; Code:

(defconst eaf-extra-packages '(eaf))

(defun eaf-extra/pre-init-eaf ()
  (spacemacs/add-to-hook 'eaf-mode-hook '(hidden-mode-line-mode))
  (spacemacs|use-package-add-hook eaf
    :pre-init
    (setq eaf-apps-to-install
          '(airshare
            browser
            camera
            pdf-viewer
            music-player
            video-player
            image-viewer
            terminal
            rss-reader
            markdown-previewer
            org-previewer
            file-manager
            file-browser
            file-sender
            git
            ;; mindmap
            ;; markmap
            ;; netease-cloud-music
            ;; js-video-player
            ;; system-monitor
            ;; map
            ))
    (setq eaf-apps
          '(airshare
            browser
            camera
            pdf-viewer
            music-player
            video-player
            image-viewer
            terminal
            rss-reader
            markdown-previewer
            org-previewer
            file-manager
            file-browser
            file-sender
            git
            ;; mindmap
            ;; markmap
            ;; netease-cloud-music
            ;; js-video-player
            ;; system-monitor
            ;; map
            ))
    :post-config
    (require 'eaf-airshare)
    (require 'eaf-browser)
    (require 'eaf-terminal)
    (require 'eaf-camera)
    (require 'eaf-video-player)
    (require 'eaf-file-manager)
    (require 'eaf-file-browser)
    (require 'eaf-file-sender)
    (require 'eaf-image-viewer)
    (require 'eaf-pdf-viewer)
    (require 'eaf-music-player)
    (require 'eaf-org-previewer)
    (require 'eaf-markdown-previewer)
    (require 'eaf-rss-reader)
    (require 'eaf-git)
    ;; (require 'eaf-system-monitor)
    ;; (require 'eaf-js-video-player)
    ;; (require 'eaf-map)
    ;; (require 'eaf-markmap)
    ;; (require 'eaf-jupyter)
    ;; (require 'eaf-2048)
    ;; (require 'eaf-netease-cloud-music)
    ;; (require 'eaf-mindmap)
    ;;-------------------------------------
    (require 'eaf-org)
    ;; eaf-terminal
    (setq eaf-terminal-font-family "FiraCode Nerd Font"
          eaf-terminal-font-size 14)
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
    ;; broot
    (add-to-list 'eaf-terminal-keybinding '("M-<return>" . "eaf-send-key-sequence"))
    (add-to-list 'eaf-terminal-keybinding '("C-<left>" . "eaf-send-key-sequence"))
    (add-to-list 'eaf-terminal-keybinding '("C-<right>" . "eaf-send-key-sequence"))
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
    (add-to-list 'eaf-browser-keybinding '("C-c l" . "org-store-link"))
    (add-to-list 'eaf-browser-keybinding '("C-c C-l" . "eaf-org-store-link"))
    (add-to-list 'eaf-browser-keybinding '("C-m" . "eaf-send-return-key"))
    (setq eaf-browser-keybinding (delete '("M-m" . "eaf-send-return-key") eaf-browser-keybinding))

    (xy/set-eaf-browser-as-default)
    ;; (xy/set-google-chrome-as-default)

    ;;------------------------------------------
    ;; eaf-camera
    (add-to-list 'eaf-camera-keybinding
                 '("x" . "eaf-py-proxy-insert_or_close_buffer"))

    ;;-------------------------------------------
    ;; eaf-file-manager
    (add-to-list 'eaf-file-manager-keybinding
                 '("C-q" . "eaf-file-sender-qrcode"))

    ;;-------------------------------------------
    ;; eaf-file-browser

    ))
