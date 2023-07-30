;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- media layer packages file for Spacemacs.
;; Time-stamp: <2023-07-29 Sat 08:52 by xin on tufg>
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
          emms-playlist-mode-window-width 50
          )
    ))

(defun media/init-emms-info-mediainfo ()
  (use-package emms-info-mediainfo))

(defun media/init-org-emms ()
  (use-package org-emms))

(defun media/init-subed ()
  (use-package subed
    :init
    ;; Disable automatic movement of point by default
    ;; (add-hook 'subed-mode-hook 'subed-disable-sync-point-to-player)
    ;; Remember cursor position between sessions
    (add-hook 'subed-mode-hook 'save-place-local-mode)))

(defun media/init-mpvi ()
  (use-package mpvi
    :config
    (setq mpvi-favor-paths '("~/视频"
                             "~/音乐"
                             "~/zbox_sshfs"
                             "/media/xin")
          mpvi-attach-link-attrs "#+attr_html: :width 320"
          ;; mpvi-ytdlp-extra-args "-c '~/.cache/cookies/cookies.txt'"
          ;; ;; NOTE: no need to open html links with mpvi
          ;; mpvi-org-https-link-rules '("www.bilibili.com/"
          ;;                             "www.youtube.com/"
          ;;                             "www.youku.com/"
          ;;                             "www.pronhub.com/"
          ;;                             )
          )
    ))

(defun media/init-bilibili ()
  (use-package bilibili
    :defer t
    :config
    (defconst bilibili-cookie-file "~/.cache/cookies/bilibili.string")
    (if (file-exists-p bilibili-cookie-file)
        (setq bilibili-cookie-text (get-string-from-file bilibili-cookie-file)))
    ))
