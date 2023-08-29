;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- lazycat Layer keybindings File for Spacemacs
;; Time-stamp: <2023-08-29 Tue 09:01 by xin on tufg>
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
  "oo"  'spacemacs/watch-other-window-transient-state/body)

(global-set-key (kbd "C-c C-v") 'spacemacs/watch-other-window-transient-state/body)
