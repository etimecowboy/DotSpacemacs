;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- eaf-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-02-22 Wed 13:04 by xin on tufg>
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
  "aar" 'eaf-open-rss-reader
  "aag" 'eaf-open-git
  "aap" 'eaf-open-pdf-from-history
  "aai" 'eaf-open-ipython)
