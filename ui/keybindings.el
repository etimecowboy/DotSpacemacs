;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- ui Layer keybindings File for Spacemacs
;; Time-stamp: <2023-07-25 Tue 02:12 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; (spacemacs/set-leader-keys
;;   "w C-<left>" 'dockwin-toggle-left-window
;;   "w C-<right>" 'dockwin-toggle-right-window
;;   "w C-<up>" 'dockwin-toggle-top-window
;;   "w C-<down>" 'dockwin-toggle-bottom-window)

(spacemacs/set-leader-keys
  "wz" popwin:keymap)

;; (global-set-key (kbd "C-z") popwin:keymap)
