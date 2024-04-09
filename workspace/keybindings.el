;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- workspace Layer keybindings File
;; Time-stamp: <2024-04-09 Tue 10:09 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; Defined in ui layer
;; (spacemacs/declare-prefix "l" "layouts")
;; (spacemacs/declare-prefix "lw" "window")

(spacemacs/set-leader-keys
  "Tw" 'burly-tabs-mode
  "ls" 'xy/workspace-save
  "lS" 'xy/workspace-save-all
  "lR" 'xy/workspace-restore
  "lW" 'burly-bookmark-windows
  "lF" 'burly-bookmark-frames
  "lB" 'burly-open-bookmark
  "lL" 'burly-open-last-bookmark
  "lV" 'burly-tabs-reset-tab
  "l C-o" 'burly-open-url
  "l C-w" 'burly-kill-windows-url
  "l C-f" 'burly-kill-frames-url
  "l C-b" 'burly-kill-buffer-url
  )

(global-set-key (kbd "<f12>") 'xy/workspace-restore)
(global-set-key (kbd "S-<f12>") 'xy/workspace-save)
(global-set-key (kbd "C-c C-w s") 'xy/workspace-save)
(global-set-key (kbd "C-c C-w S") 'xy/workspace-save-all)
(global-set-key (kbd "C-c C-w R") 'xy/workspace-restore)
(global-set-key (kbd "C-c C-w W") 'burly-bookmark-windows)
(global-set-key (kbd "C-c C-w F") 'burly-bookmark-frames)
(global-set-key (kbd "C-c C-w B") 'burly-open-bookmark)
(global-set-key (kbd "C-c C-w L") 'burly-open-last-bookmark)
(global-set-key (kbd "C-c C-w V") 'burly-tabs-reset-tab)
(global-set-key (kbd "C-c C-w C-o") 'burly-open-url)
(global-set-key (kbd "C-c C-w C-w") 'burly-kill-windows-url)
(global-set-key (kbd "C-c C-w C-f") 'burly-kill-frames-url)
(global-set-key (kbd "C-c C-w C-b") 'burly-kill-buffer-url)

;; (spacemacs/set-leader-keys
;;   "l"  'spacemacs/workspaces-transient-state/body
;;   "tZ" 'desktop-save-mode
;;   "tE" 'eyebrowse-restore-mode)

;; (global-set-key (kbd "C-c C-w C-r") 'xy/workspace-restore)
;; (global-set-key (kbd "C-c C-w C-s") 'xy/workspace-save)
;; (global-set-key (kbd "C-c C-w n") 'xy/set-frame-name)
;; (global-set-key (kbd "C-c C-w f") 'set-frame-by-name)
;; (global-set-key (kbd "C-c C-w s") 'desktop-save)
;; (global-set-key (kbd "C-c C-w r") 'desktop-read)
;; (global-set-key (kbd "C-c C-w c") 'desktop-clear)
;; (global-set-key (kbd "C-c C-w v") 'desktop-revert)
;; (global-set-key (kbd "C-c C-w d") 'desktop-remove)
;; (global-set-key (kbd "C-c C-w D") 'desktop-change-dir)
