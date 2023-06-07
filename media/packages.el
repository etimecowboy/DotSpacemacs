;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- media layer packages file for Spacemacs.
;; Time-stamp: <2023-06-04 Sun 08:43 by xin on tufg>
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
  (use-package emms)
  :config
  (emms-all)
  (emms-default-players)
  (require 'emms-player-simple)
  (setq emms-source-file-default-directory "~/音乐"
        emms-playlist-mode-center-when-go t
        emms-volume-change-function 'emms-volume-pulse-change))

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
                             "~/zbox_sshfs"
                             "/media/xin")
          mpvi-attach-link-attrs "#+attr_html: :width 320"
          ;; mpvi-ytdlp-extra-args "-c '~/.cache/cookies/cookies.txt'"
          mpvi-org-https-link-rules '("www.bilibili.com/"
                                      "www.youtube.com/"
                                      "www.youku.com/"
                                      "www.pronhub.com/"
                                      ))
    ))

(defun media/init-bilibili ()
  (use-package bilibili
    :defer t
    :config
    (defconst bilibili-cookie-file "~/.cache/cookies/bilibili.string")
    (if (file-exists-p bilibili-cookie-file)
        (setq bilibili-cookie-text (get-string-from-file bilibili-cookie-file)))
    ))
