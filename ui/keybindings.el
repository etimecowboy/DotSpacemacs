;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- ui Layer keybindings File for Spacemacs
;; Time-stamp: <2025-11-16 Sun 10:34:32 GMT by xin on tufg>
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
  "TH" 'breadcrumb-mode
  "Ta" 'toggle-frame-tab-bar
  "Tl" 'tab-line-mode
  "TL" 'global-tab-line-mode
  "Tx" 'global-tabs-mode
  "Ti" 'which-key-posframe-mode
  "T1" 'xy/ide-gui
  "T2" 'xy/tabs-gui
  "T3" 'xy/default-gui
  "T4" 'xy/mini-gui

  ;; "Ww0" 'tab-line-close-tab
  ;; "Th" 'xy/toggle-header-line
  ;; "Th" 'path-headerline-mode
  "fz" 'xy/show-file-name
  )

(global-set-key (kbd "C-x t c") 'tab-line-new-tab)
(global-set-key (kbd "C-x t w") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-x t W") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-x t z") 'xy/show-file-name)


(global-set-key (kbd "<f6>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "<f7>") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "S-<f6>") 'tab-previous)
(global-set-key (kbd "S-<f7>") 'tab-next)
(global-set-key (kbd "C-<f6>") 'bury-buffer)
(global-set-key (kbd "C-S-<f6>") 'tab-close)
(global-set-key (kbd "C-<f7>") 'tab-detach)
(global-set-key (kbd "C-S-<f7>") 'tab-window-detach)

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

;; (spacemacs/set-leader-keys "wpm" 'popwin:messages)
;; (spacemacs/set-leader-keys "wpp" 'popwin:close-popup-window)
;; (spacemacs/set-leader-keys "rw" 'spacemacs/last-popwin)

(spacemacs/set-leader-keys "wpb" 'popwin:popup-buffer)
(spacemacs/set-leader-keys "wpf" 'popwin:find-file)
(spacemacs/set-leader-keys "wpt" 'popwin:popup-buffer-tail)
(spacemacs/set-leader-keys "wpT" 'popwin:find-file-tail)
(spacemacs/set-leader-keys "wpl" 'popwin:display-last-buffer)
(spacemacs/set-leader-keys "wpg" 'popwin:close-popup-window)
(spacemacs/set-leader-keys "wpk" 'popwin:close-popup-window)
(spacemacs/set-leader-keys "wpq" 'popwin:close-popup-window)
