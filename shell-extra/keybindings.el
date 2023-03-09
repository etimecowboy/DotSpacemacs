;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- shell-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-03-09 Thu 02:41 by xin on tufg>
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
  "atsV" 'multi-vterm
  "atsP" 'multi-vterm-project
  "atsb" 'vterm-send-ctrl-b
  "atsg" 'vterm-send-ctrl-g)
