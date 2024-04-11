;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- ui Layer keybindings File for Spacemacs
;; Time-stamp: <2024-04-10 Wed 15:34 by xin on tufg>
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
  "Ta" 'toggle-frame-tab-bar
  "Tl" 'tab-line-mode
  "TL" 'global-tab-line-mode
  "Tx" 'global-tabs-mode
  "T1" 'xy/ide-gui
  "T2" 'xy/tabs-gui
  "T3" 'xy/default-gui
  "T4" 'xy/mini-gui
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
  "fz" 'xy/show-file-name
  )

(global-set-key (kbd "C-x t c") 'tab-line-new-tab)
(global-set-key (kbd "C-x t w") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-x t W") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-x t z") 'xy/show-file-name)
(global-set-key (kbd "<f6>") 'tab-previous)
(global-set-key (kbd "<f7>") 'tab-next)
(global-set-key (kbd "S-<f6>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "S-<f7>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-<f6>") 'tab-new)
(global-set-key (kbd "C-<f7>") 'breadcrumb-mode)
(global-set-key (kbd "M-<f7>") 'global-tabs-mode)
(global-set-key (kbd "C-S-<f6>") 'tab-close)
(global-set-key (kbd "C-S-<f7>") 'bury-buffer)

;; NOTE: { M-<f6> } and { M-S-<f6> } are preserved by GNOME for selecting same
;; app window.

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

;; NOTE: "S-<f11>" is replaced by `xy/toggle-demo'
;; (global-set-key (kbd "S-<f11>") 'xy/restore-frame-size)
(global-set-key (kbd "C-<f11>") 'xy/tabs-gui)
(global-set-key (kbd "M-<f11>") 'xy/mini-gui)
(global-set-key (kbd "C-S-<f11>") 'xy/ide-gui)
(global-set-key (kbd "C-M-<f11>") 'xy/default-gui)

;; NOTE: <f11> is full screen toggle key in Ubuntu GNOME shell, which is not
;; that useful. I configured it as 'xy/toogle-my-focus'.
