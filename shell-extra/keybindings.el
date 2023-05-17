;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- shell-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-05-15 Mon 15:16 by xin on tufg>
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
  "atsa" 'aweshell-dedicated-toggle
  "atsc" 'aweshell-new
  "atsp" 'aweshell-prev
  "atsn" 'aweshell-next
  "atsV" 'multi-vterm-dedicated-toggle
  "atsu" 'multi-vterm
  "atsP" 'multi-vterm-project)

(spacemacs/set-leader-keys-for-major-mode 'vterm-mode
  "g" 'vterm-send-ctrl-g
  "b" 'vterm-send-ctrl-b
  )
