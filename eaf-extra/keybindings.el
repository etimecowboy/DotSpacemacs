;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- eaf-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-03-11 Sat 15:05 by xin on tufg>
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
  "tb" 'xy/toggle-eaf-browser
  "aabt" 'xy/toggle-eaf-browser
  "aabp" 'eaf-open-url-at-point
  "aabO" 'eaf-open-browser-other-window
  "aabB" 'eaf-open-browser-in-background
  "aaa"  'browse-url-at-point
  "aaA"  'browse-url-chrome
  "aae"  'eaf-open-url-at-point
  "aar" 'eaf-open-rss-reader
  ;; "aag" 'eaf-open-git
  "aap" 'eaf-open-pdf-from-history
  ;; "aai" 'eaf-open-ipython
  ;; "aax" 'eaf-open-map
  )
