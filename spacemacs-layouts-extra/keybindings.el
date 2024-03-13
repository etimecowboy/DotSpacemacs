;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- spacemacs-layouts-extra Layer keybindings File
;; Time-stamp: <2024-03-09 Sat 02:17 by xin on tufg>
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
  "l"  'spacemacs/workspaces-transient-state/body
  "tZ" 'desktop-save-mode
  "tE" 'eyebrowse-restore-mode
  )
