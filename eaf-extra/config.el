;;; config.el --- eaf-extra configuration File for Spacemacs
;; Time-stamp: <2023-03-03 Fri 22:41 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:
(with-eval-after-load "eaf"
  (setq eaf-apps-to-install
   '(browser
     pdf-viewer
     music-player
     video-player
     image-viewer
     system-monitor
     terminal
     git
     map
     rss-reader
     mindmap
     markmap
     markdown-previewer
     org-previewer
     ;;file-manager
     ;;file-browser
     ))
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-music-player)
  (require 'eaf-video-player)
  (require 'eaf-image-viewer)
  (require 'eaf-system-monitor)
  (require 'eaf-terminal)
  (require 'eaf-git)
  (require 'eaf-map)
  (require 'eaf-rss-reader)
  (require 'eaf-mindmap)
  (require 'eaf-markmap)
  (require 'eaf-markdown-previewer)
  (require 'eaf-org-previewer)
  (require 'eaf-org-previewer)
  ;; (require 'eaf-file-manager)
  ;; (require 'eaf-file-browser)

  ;;-------------------------------------
  (require 'eaf-org)
  ;;-------------------------------------
  ;; eaf-terminal
  ;; override function eaf-ipython-command to use ipython3
  (defun eaf-ipython-command ()
    (if (eaf--called-from-wsl-on-windows-p)
        "ipython.exe"
      "ipython3"))
  (setq eaf-terminal-font-family "Cascadia Code"
        eaf-terminal-font-size 14)
  (add-to-list 'eaf-terminal-keybinding
               '("M-z" . "eaf-send-key-sequence"))
  (add-to-list 'eaf-terminal-keybinding
               '("C-\\" . "eaf-send-key-sequence"))

  ;;---------------------------------------
  ;; eaf-browser
  ;; 设定eaf默认搜索引擎
  (setq eaf-browser-default-search-engine "google")
  ;; 设定eaf开启广告屏蔽器
  (setq eaf-browser-enable-adblocker t)
  ;; 设定eaf浏览器的缩放
  ;; (setq eaf-browser-default-zoom 1.2)
  ;; (eaf-setq eaf-browser-enable-adblocker "true")
  ;; set proxy
  (setq eaf-proxy-type "socks5"
        eaf-proxy-host "127.0.0.1"
        eaf-proxy-port "7890")
  (add-to-list 'eaf-browser-keybinding '("C" . "xy/open-current-webpage-in-chrome"))
  (add-to-list 'eaf-browser-keybinding '("w" . "eaf-get-path-or-url"))
  (add-to-list 'eaf-browser-keybinding '("C-c C-l" . "eaf-org-store-link"))
  (xy/set-eaf-browser-as-default)
  )
