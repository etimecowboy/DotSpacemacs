;;; packages.el --- eaf-extra layer packages File for Spacemacs
;; Time-stamp: <2023-06-28 Wed 07:30 by xin on tufg>
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

(defconst eaf-extra-packages
  '(eaf
    ;; (eaf-terminal-dedicated :location local)
    ))

(defun eaf-extra/pre-init-eaf ()
  (spacemacs/add-to-hook 'eaf-mode-hook '(hidden-mode-line-mode))
  (spacemacs|use-package-add-hook eaf
    :pre-init
    (setq eaf-apps-to-install
          '(
            browser
            camera
            pdf-viewer
            image-viewer
            pyqterminal
            ;; terminal
            ;; airshare
            ;; file-browser
            ;; file-sender
            ;; music-player
            ;; video-player
            ;; rss-reader
            ;; markdown-previewer
            ;; org-previewer
            ;; file-manager
            ;; git
            ;; mindmap
            ;; markmap
            ;; netease-cloud-music
            ;; js-video-player
            ;; system-monitor
            ;; map
            ))
    (setq eaf-apps
          '(
            browser
            camera
            pdf-viewer
            image-viewer
	          pyqterminal
            ;; terminal
            ;; file-browser
            ;; file-sender
            ;; airshare
            ;; music-player
            ;; video-player
            ;; rss-reader
            ;; markdown-previewer
            ;; org-previewer
            ;; file-manager
            ;; git
            ;; mindmap
            ;; markmap
            ;; netease-cloud-music
            ;; js-video-player
            ;; system-monitor
            ;; map
            ))
    :post-config
    ;; always use a conda env to run eaf apps
    (require 'conda)
    (setq-default eaf-python-command "/home/xin/.conda/envs/py310_emacs/bin/python")
    (conda-env-activate "py310_emacs")

    ;; load eaf apps
    (require 'eaf-browser)
    (require 'eaf-image-viewer)
    (require 'eaf-pdf-viewer)
    (require 'eaf-camera)
    (require 'eaf-pyqterminal)
    ;; (require 'eaf-terminal)
    ;; (require 'eaf-airshare)
    ;; (require 'eaf-file-browser)
    ;; (require 'eaf-file-sender)
    ;; (require 'eaf-video-player)
    ;; (require 'eaf-file-manager)
    ;; (require 'eaf-music-player)
    ;; (require 'eaf-org-previewer)
    ;; (require 'eaf-markdown-previewer)
    ;; (require 'eaf-rss-reader)
    ;; (require 'eaf-git)
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
    (require 'eaf-all-the-icons)
    ;; ;;-------------------------------------
    ;; ;; eaf-terminal
    ;; ;;---------------------
    ;; ;;-- Add pop-shell choice
    ;; ;; FIXME: mode-line string changed to *Default-efa-terminal-N*,
    ;; ;; and cannot changed back to current buffer name
    ;; (make-shell-pop-command "eaf-terminal" eaf-terminal)
    ;; (spacemacs/set-leader-keys
    ;;   "atsw" 'spacemacs/shell-pop-eaf-terminal)
    ;; (spacemacs/register-repl 'eaf-terminal 'eaf-terminal)
    ;; ;;---------------------
    ;; (setq eaf-terminal-font-family "Sarasa Mono SC Nerd Font" ;; "FiraCode Nerd Font"
    ;;       eaf-terminal-font-size 14)
    ;; (add-list-to-list 'eaf-terminal-keybinding
    ;;                   ;; my tmux prefix M-z
    ;;                   '(("M-z" . "eaf-send-key-sequence")
    ;;                     ;; fzf
    ;;                     ("C-r" . "eaf-send-key-sequence")
    ;;                     ("C-t" . "eaf-send-key-sequence")
    ;;                     ("M-c" . "eaf-send-key-sequence")
    ;;                     ("M-t" . "eaf-send-key-sequence")
    ;;                     ("M-e" . "eaf-send-key-sequence")
    ;;                     ;; mc
    ;;                     ("C-\\" . "eaf-send-key-sequence")
    ;;                     ("M-i" .  "eaf-send-key-sequence")
    ;;                     ;; zellij
    ;;                     ("C-g" . "eaf-send-key-sequence")
    ;;                     ;; ("C-t" . "eaf-send-key-sequence") ;; duplicated
    ;;                     ("C-s" . "eaf-send-key-sequence")
    ;;                     ("C-h" . "eaf-send-key-sequence")
    ;;                     ("C-q" . "eaf-send-key-sequence")
    ;;                     ("M-n" . "eaf-send-key-sequence")
    ;;                     ;; ("M-<up>" . "eaf-send-key-sequence")
    ;;                     ;; ("M-<down>" . "eaf-send-key-sequence")
    ;;                     ;; ("M-<left>" . "eaf-send-key-sequence")
    ;;                     ;; ("M-<right>" . "eaf-send-key-sequence")
    ;;                     ("M-h" . "eaf-send-key-sequence")
    ;;                     ("M-j" . "eaf-send-key-sequence")
    ;;                     ("M-k" . "eaf-send-key-sequence")
    ;;                     ("M-l" . "eaf-send-key-sequence")
    ;;                     ;; ("M-+" . "eaf-send-key-sequence")
    ;;                     ;; ("M--" . "eaf-send-key-sequence")
    ;;                     ;; broot
    ;;                     ("M-<return>" . "eaf-send-key-sequence")
    ;;                     ("C-<left>" . "eaf-send-key-sequence")
    ;;                     ("C-<right>" . "eaf-send-key-sequence")
    ;;                     ))

    ;;---------------------------------------
    ;; eaf-pyqterminal
    (setq eaf-pyqterminal-font-size 16
          eaf-pyqterminal-font-family "Sarasa Term SC Nerd Font" ;;"FiraCode Nerd Font"
          ;; eaf-pyqterminal-font-family "Sarasa Mono SC Nerd Font"
          )
    (add-list-to-list 'eaf-pyqterminal-keybinding
                      ;; my tmux prefix M-z
                      '(("M-z" . "eaf-send-key-sequence")
                        ;; fzf
                        ("C-r" . "eaf-send-key-sequence")
                        ("C-t" . "eaf-send-key-sequence")
                        ("M-c" . "eaf-send-key-sequence")
                        ("M-t" . "eaf-send-key-sequence")
                        ("M-e" . "eaf-send-key-sequence")
                        ;; mc
                        ("C-\\" . "eaf-send-key-sequence")
                        ("M-i" .  "eaf-send-key-sequence")
                        ;; zellij
                        ("C-g" . "eaf-send-key-sequence")
                        ;; ("C-t" . "eaf-send-key-sequence") ;; duplicated
                        ("C-s" . "eaf-send-key-sequence")
                        ("C-h" . "eaf-send-key-sequence")
                        ("C-q" . "eaf-send-key-sequence")
                        ("M-n" . "eaf-send-key-sequence")
                        ;; ("M-<up>" . "eaf-send-key-sequence")
                        ;; ("M-<down>" . "eaf-send-key-sequence")
                        ;; ("M-<left>" . "eaf-send-key-sequence")
                        ;; ("M-<right>" . "eaf-send-key-sequence")
                        ("M-h" . "eaf-send-key-sequence")
                        ("M-j" . "eaf-send-key-sequence")
                        ("M-k" . "eaf-send-key-sequence")
                        ("M-l" . "eaf-send-key-sequence")
                        ;; ("M-+" . "eaf-send-key-sequence")
                        ;; ("M--" . "eaf-send-key-sequence")
                        ;; broot
                        ("M-<return>" . "eaf-send-key-sequence")
                        ("C-<left>" . "eaf-send-key-sequence")
                        ("C-<right>" . "eaf-send-key-sequence")
                        ))
    ;;----------------------------------------
    ;; eaf-browser
    (setq eaf-browser-default-search-engine "google" ;; 设定eaf默认搜索引擎
          eaf-browser-enable-adblocker t   ;; 设定eaf开启广告屏蔽器
          eaf-browser-default-zoom 1.2 ;; 设定eaf浏览器的缩放
          eaf-webengine-download-path "~/下载/eaf-browser"
          eaf-proxy-type "socks5"
          eaf-proxy-host "127.0.0.1"
          eaf-proxy-port "7890")
    (add-list-to-list 'eaf-browser-keybinding
                      '(("C" . "xy/open-current-webpage-in-chrome")
                        ("E" . "xy/open-current-webpage-in-eww")
                        ("w" . "eaf-get-path-or-url")
                        ("C-c l" . "org-store-link")
                        ("C-c C-l" . "eaf-org-store-link")
                        ("C-m" . "eaf-send-return-key")
                        ))
    (setq eaf-browser-keybinding
          (delete '("M-m" . "eaf-send-return-key") eaf-browser-keybinding))
    (add-to-list 'eaf-browser-keybinding '("SPC" . "nil"))
    ;; (setq eaf-browser-keybinding
    ;;       (delete '("<SPC>" . "insert_or_scroll_up_page") eaf-browser-keybinding))
    ;; (add-to-list 'eaf-browser-keybinding '("<SPC>" . nil))

    ;; set in .spacemacs
    ;; (xy/set-eaf-browser-as-default)
    ;; (xy/set-google-chrome-as-default)

    ;;------------------------------------------
    ;; eaf-camera
    ;; (add-to-list 'eaf-camera-keybinding
    ;;              ;; '("x" . "eaf-py-proxy-insert_or_close_buffer")
    ;;              ;; deprecated
    ;;              ;; '("x" . "eaf-")
    ;;              ;; '("f" . "eaf-toggle-fullscreen") ;; not working
    ;;              )
    (setq eaf-camera-save-path "~/图片/截图/")

    ;;-------------------------------------------
    ;; eaf-file-manager
    ;; (add-to-list 'eaf-file-manager-keybinding
    ;;              '("C-q" . "eaf-file-sender-qrcode"))

    ;;-------------------------------------------
    ;; Remove some advices
    (advice-remove 'dired-find-file #'eaf--dired-find-file-advisor)
    (advice-remove 'dired-find-alternate-file #'eaf--dired-find-file-advisor)
    ))

;; (defun eaf-extra/init-eaf-terminal-dedicated ()
;;   (use-package eaf-terminal-dedicated
;;     :ensure t
;;     :after eaf-terminal
;;     )
;;   )
