;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- ui Layer keybindings File for Spacemacs
;; Time-stamp: <2024-03-28 Thu 11:04 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/declare-prefix "l" "workspace")
;; (spacemacs/declare-prefix "lw" "window")

(spacemacs/set-leader-keys
  "wz" 'popwin:keymap
  "Tp" 'spacious-padding-mode
  "Tb" 'spacemacs/toggle-mode-line
  "Th" 'breadcrumb-local-mode
  "TH" 'breadcrumb-mode
  "Tz" 'xy/toggle-my-focus
  "Tr" 'xy/restore-frame-size
  "Ta" 'toggle-frame-tab-bar
  "Tl" 'tab-line-mode
  "TL" 'global-tab-line-mode
  "TL" 'global-tab-line-mode
  "Tx" 'xy/turn-off-tabs
  "TX" 'xy/turn-on-tabs
  "lt"  'tab-new
  "lT"  'tab-new-to
  "l C-f" 'find-file-other-tab
  "l C-r" 'find-file-read-only-other-tab
  "lo"  'tab-next
  "lO"  'tab-previous
  "l RET" 'tab-switch
  "lu"  'tab-undo
  "lm"  'tab-move
  "lM"  'tab-move-to
  "l0"  'tab-close
  "l1"  'tab-close-other
  "lr"  'tab-rename
  "lw"  'tab-window-detach
  "ld"  'tab-detach
  "lc"  'tab-duplicate
  "lG"  'tab-group
  "lD"  'dired-other-tab
  "lx"  'other-tab-prefix
  "lP"  'project-other-tab-command
  "ll" 'tab-line-new-tab
  "lf" 'tab-line-switch-to-next-tab
  "lb" 'tab-line-switch-to-prev-tab
  ;; "lw0" 'tab-line-close-tab
  ;; "Th" 'xy/toggle-header-line
  ;; "Th" 'path-headerline-mode
  "tV" 'visual-line-mode
  "t C-v" 'global-visual-line-mode
  "tC" 'visual-fill-column-mode
  "t C-c" 'global-visual-fill-column-mode
  "tA" 'adaptive-wrap-prefix-mode
  )

(global-set-key (kbd "C-c z") 'xy/show-file-name)

;; Complete window resizing key
;;
;; exsiting keys:
;;
;;   - { C-x ^ } `enlarge-window'
;;   - { C-x \} } `enlarge-window-horizontally'
;;   - { C-x \{ } `shrink-window-horizontally'
(global-set-key (kbd "C-x %") 'shrink-window)  ;; `shrink-window'

;; (spacemacs/set-leader-keys
;;   "w C-<left>" 'dockwin-toggle-left-window
;;   "w C-<right>" 'dockwin-toggle-right-window
;;   "w C-<up>" 'dockwin-toggle-top-window
;;   "w C-<down>" 'dockwin-toggle-bottom-window)

;; (global-set-key (kbd "C-z") popwin:keymap)

(global-set-key (kbd "<f11>") 'xy/toogle-my-focus)
;; NOTE: <f11> is full screen toggle key in Ubuntu GNOME shell, which is not
;; that useful. I configured it as 'xy/toogle-my-focus'.
