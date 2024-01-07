;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- ui Layer keybindings File for Spacemacs
;; Time-stamp: <2024-01-07 Sun 05:03 by xin on tufg>
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
  "wz" 'popwin:keymap
  "Tp" 'spacious-padding-mode
  "Tb" 'spacemacs/toggle-mode-line
  "Th" 'breadcrumb-local-mode
  ;; "Th" 'xy/toggle-header-line
  ;; "Th" 'path-headerline-mode
  "hb" 'esup
  )

(global-set-key (kbd "C-c z") 'xy/show-file-name)

;; (spacemacs/set-leader-keys
;;   "w C-<left>" 'dockwin-toggle-left-window
;;   "w C-<right>" 'dockwin-toggle-right-window
;;   "w C-<up>" 'dockwin-toggle-top-window
;;   "w C-<down>" 'dockwin-toggle-bottom-window)

;; (global-set-key (kbd "C-z") popwin:keymap)
