;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- media layer packages file for Spacemacs.
;; Time-stamp: <2025-10-27 Mon 11:03:27 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst media-packages
  '(emms
    emms-info-mediainfo
    org-emms
    subed
    (mpvi :location (recipe :fetcher github :repo "lorniu/mpvi"))
    (bilibili :location (recipe :fetcher github :repo "lorniu/bilibili.el"))
    ))

(defun media/init-emms ()
  (use-package emms
    :init
    (spacemacs|define-transient-state emms
      :title "emms transient state"
      :doc "
    ^Control^                    ^Playlist^                    ^Misc^
    ^^^^^^^^-------------------------------------------------------------------------
    [_SPC_] pause                [_p_] play-playlist           [_b_] browse library
    [_RET_/_ESC_] stop             [_c_] create-new              [_t_] show info
    [_l_/_<right>_] forward        [_j_/_<down>_] next             [_m_] show status
    [_h_/_<left>_]  backward       [_k_/_<up>_] previous           [_L_] load history
    [_:_] seek-to                [_r_] random                  [_H_] save history
    [_-_] volume-lower           [_R_] shuffle                 [_q_] quit
    [_=_/_+_] volume-raise         [_g_] show playlist
    [_f_] play-file              [_G_] popup playlist
    [_d_] play-directory
    "
      :bindings
      ("q" nil :exit t)
      ("j" emms-next)
      ("<down>" emms-next)
      ("k" emms-previous)
      ("<up>" emms-previous)
      ("h" emms-seek-backward)
      ("<left>" emms-seek-backward)
      ("l" emms-seek-forward)
      ("<right>" emms-seek-forward)
      (":" emms-seek-to)
      ("-" emms-volume-lower)
      ("=" emms-volume-raise)
      ("+" emms-volume-raise)
      ("t" emms-show)
      ("m" emms-display-modes)
      ("p" emms-play-playlist)
      ("f" emms-play-file)
      ("d" emms-play-directory)
      ("g" emms-playlist-mode-go)
      ("G" emms-playlist-mode-popup)
      ("b" emms-browser)
      ("R" emms-shuffle)
      ("r" emms-random)
      ("c" emms-playlist-new)
      ("L" emms-history-load)
      ("H" emms-history-save)
      ("SPC" emms-pause)
      ("RET" emms-stop)
      ("ESC" emms-stop)
      )

    :config
    (require 'emms-setup)
    (require 'emms-player-simple)
    (require 'emms-history)
    (emms-all)
    (emms-default-players)
    (emms-history-load)
    (setq emms-source-file-default-directory "~/音乐"
          emms-volume-change-function 'emms-volume-pulse-change
          emms-player-list '(emms-player-mpv)
          emms-player-mpv-use-playlist-option t
          emms-playlist-mode-center-when-go t
          emms-playlist-mode-window-width 50)
    ))

(defun media/init-emms-info-mediainfo ()
  (use-package emms-info-mediainfo))

(defun media/init-org-emms ()
  (use-package org-emms))

(defun media/init-subed ()
  (use-package subed
    :config
    ;; Remember cursor position between sessions
    (add-hook 'subed-mode-hook 'save-place-local-mode)
    ;; Break lines automatically while typing
    (add-hook 'subed-mode-hook 'turn-on-auto-fill)
    ;; Break lines at 40 characters
    (add-hook 'subed-mode-hook (lambda () (setq-local fill-column 40)))
    ;; Some reasonable defaults
    (add-hook 'subed-mode-hook 'subed-enable-pause-while-typing)
    ;; As the player moves, update the point to show the current subtitle
    (add-hook 'subed-mode-hook 'subed-enable-sync-point-to-player)
    ;; Disable automatic movement of point by default
    ;; (add-hook 'subed-mode-hook 'subed-disable-sync-point-to-player)
    ;; As your point moves in Emacs, update the player to start at the current subtitle
    (add-hook 'subed-mode-hook 'subed-enable-sync-player-to-point)
    ;; Replay subtitles as you adjust their start or stop time with M-[, M-], M-{, or M-}
    (add-hook 'subed-mode-hook 'subed-enable-replay-adjusted-subtitle)
    ;; Loop over subtitles
    (add-hook 'subed-mode-hook 'subed-enable-loop-over-current-subtitle)
    ;; Show characters per second
    (add-hook 'subed-mode-hook 'subed-enable-show-cps)
    :custom
    (subed-auto-play-media nil)
    ))

(defun media/init-mpvi ()
  (use-package mpvi
    :after (org popwin)
    ;; :ensure t
    ;; :init
    ;; (add-to-list 'display-buffer-alist
    ;;              '("mpvi-.*$" display-buffer-at-bottom))
    :bind
    ((:map mpvi-org-link-map
           (", c" . mpvi-control)
           (", +" . mpvi-add-emms)
           (", E" . spacemacs/emms-transient-state/body)
           (", ?" . xy/describe-keymap-mpvi-org-link))
     (:map mpvi-seek-map
           ("c" . mpvi-control)
           ("+" . mpvi-add-emms)
           ("E" . spacemacs/emms-transient-state/body)
           ("?" . xy/describe-keymap-mpvi-seek))
     (:map mpvi-control-map
           ("+" . mpvi-add-emms)
           ("E" . spacemacs/emms-transient-state/body)
           ("?" . xy/describe-keymap-mpvi-control)))

    :custom
    ;; (mpvi-favor-paths '("~/视频"
    ;;                     "~/音乐"
    ;;                     "~/zbox_sshfs"
    ;;                     "/media/xin"))
    ;; (mpvi-cache-directory spacemacs-cache-directory)
    ;; (mpvi-attach-link-attrs "#+ATTR_ORG: :width 240\n#+ATTR_HTML: :width 320")

    ;; (mpvi-ytdlp-extra-args "-c '~/.cache/cookies/cookies.txt'")

    ;; NOTE: no need to open html links with mpvi
    ;; (mpvi-org-https-link-rules '("bilibili"
    ;;                              "youtube"
    ;;                              "youku"))

    (mpvi-mpv-ontop-p nil)
    (mpvi-mpv-border-p t)
    (mpvi-cmds-on-init '(((set_property autofit "40%x85%"))
                         ((set_property geometry "-3%+8%"))))

    ;; Fix `mpvi-check-live' function problem after Emms updated to 18+ (2024)
    ;; (setq emms-player-mpv-ipc-method 'ipc-server)

    ;; -------------------------------------------------
    ;; FIXME: ;; Error loading autoloads
    ;; (void-variable mpvi-org-https-link-rules) ;; FAIL
    ;; (mpvi-org-https-link-rules nil)
    ;; -------------------------------------------------

    :config
    (defun xy/describe-keymap-mpvi-control ()
      "Describe keymap mpvi-control-map"
      (interactive)
      (describe-keymap 'mpvi-control-map))

    (defun xy/describe-keymap-mpvi-org-link ()
      "Describe keymap mpvi-org-link"
      (interactive)
      (describe-keymap 'mpvi-org-link-map))

    (defun xy/describe-keymap-mpvi-seek ()
      "Describe keymap mpvi-seek-map"
      (interactive)
      (describe-keymap 'mpvi-seek-map))

    ;; (with-eval-after-load 'popwin
    ;;   (push '("*mpvi-control*"
    ;;           :dedicated t
    ;;           :position bottom
    ;;           :stick t
    ;;           :noselect nil
    ;;           :height 0.4)
    ;;         popwin:special-display-config))

    (push '("*mpvi-control*"
            :dedicated t
            :position bottom
            :stick t
            ;; :noselect nil
            :height 0.2)
          popwin:special-display-config)

    ;; (push '(mpvi-control-mode
    ;;         :dedicated t
    ;;         :position bottom
    ;;         :stick t
    ;;         :noselect nil
    ;;         :height 0.4)
    ;;       popwin:special-display-config)

    ;; (with-eval-after-load 'popwin
    ;;   (push '(mpvi-control-mode
    ;;           :position bottom
    ;;           :dedicated t
    ;;           :stick t
    ;;           :noselect nil)
    ;;         popwin:special-display-config))

    (defun mpvi-org-link-push (link)
      "Play the mpv LINK."
      (pcase-let ((`(,path ,beg ,end) (mpvi-parse-link link)))
        (mpvi-start path nil beg end)))
    ))

(defun media/init-bilibili ()
  (use-package bilibili
    :defer t
    :config
    (defconst bilibili-cookie-file "~/.cache/cookies/bilibili.string")

    ;; REF: http://xahlee.info/emacs/emacs/elisp_read_file_content.html
    (defun get-string-from-file (filePath)
      "Return file content as string."
      (with-temp-buffer
        (insert-file-contents filePath)
        (buffer-string)))

    (if (file-exists-p bilibili-cookie-file)
        (setq bilibili-cookie-text (get-string-from-file bilibili-cookie-file)))
    ))
