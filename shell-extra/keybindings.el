;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- shell-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2024-01-01 Mon 03:44 by xin on tufg>
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
  "b" 'vterm-send-ctrl-b)


(global-set-key (kbd "C-<f12>") 'spacemacs/default-pop-shell)
(global-set-key (kbd "M-<f12>") 'aweshell-dedicated-toggle)
(global-set-key (kbd "S-<f12>") 'multi-vterm)
