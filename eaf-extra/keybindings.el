;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- eaf-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-05-17 Wed 09:09 by xin on tufg>
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
  "tb"   'xy/toggle-eaf-browser
  "aabt" 'xy/toggle-eaf-browser
  "aabp" 'eaf-open-url-at-point
  "aabO" 'eaf-open-browser-other-window
  "aabB" 'eaf-open-browser-in-background
  "aaa"  'browse-url-at-point
  "aaC"  'browse-url-chrome
  "aae"  'eaf-open-url-at-point
  "aar"  'eaf-open-rss-reader
  "aap"  'eaf-open-pdf-from-history
  "aaF"  'eaf-open-file-manager
  "aaB"  'eaf-file-browser-qrcode
  "aaS"  'eaf-file-sender-qrcode
  "aaa"  'eaf-open-airshare
  "aag"  'eaf-open-git
  ;; "aai" 'eaf-open-ipython
  ;; "aax" 'eaf-open-map
  )

;; (define-key dired-mode-map (kbd "C-m") 'eaf-open-in-file-manager)
(define-key dired-mode-map (kbd "C-q") 'eaf-file-sender-qrcode-in-dired)
