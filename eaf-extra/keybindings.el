;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- eaf-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-08-05 Sat 07:48 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/set-leader-keys
  "te"   'xy/set-eaf-browser-as-default-browser
  "tG"   'xy/set-google-chrome-as-default-browser
  "tt"   'xy/set-eww-as-default-browser
  "aaB"  'eaf-open-this-buffer
  "aabp" 'eaf-open-url-at-point
  "aabO" 'eaf-open-browser-other-window
  "aabB" 'eaf-open-browser-in-background
  "aaa"  'browse-url-at-point
  "aaC"  'browse-url-chrome
  "aaE"  'xy/set-eww-as-default
  "aae"  'eaf-open-url-at-point
  "aap"  'eaf-open-pdf-from-history
  "aat"  'xy/eaf-open-tmux
  "aaT"  'eaf-open-pyqterminal
  "aaO"  'eaf-ocr-buffer
  ;; "aaB"  'eaf-file-browser-qrcode
  ;; "aaS"  'eaf-file-sender-qrcode
  ;; "aaa"  'eaf-open-airshare
  ;; "aar"  'eaf-open-rss-reader
  ;; "aaF"  'eaf-open-file-manager
  ;; "aag"  'eaf-open-git
  ;; "aai" 'eaf-open-ipython
  ;; "aax" 'eaf-open-map
  )

;; (define-key dired-mode-map (kbd "C-m") 'eaf-open-in-file-manager)
;; (define-key dired-mode-map (kbd "C-q") 'eaf-file-sender-qrcode-in-dired)
