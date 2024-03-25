;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- shell-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2024-03-17 Sun 02:23 by xin on tufg>
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
  "atsP" 'multi-vterm-project
  "&"    'async-shell-launch
  )

(spacemacs/set-leader-keys-for-major-mode 'vterm-mode
  "g" 'vterm-send-ctrl-g
  "b" 'vterm-send-ctrl-b)

(global-set-key (kbd "C-<f12>") 'xy/default-pop-shell)
(global-set-key (kbd "M-<f12>") 'xy/default-pop-tmux)
(global-set-key (kbd "C-M-<f12>") 'aweshell-dedicated-toggle)
(global-set-key (kbd "S-<f12>") 'multi-vterm)
