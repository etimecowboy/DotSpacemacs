;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- workspace Layer keybindings File
;; Time-stamp: <2024-03-25 Mon 03:30 by xin on tufg>
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
  "lr" 'xy/workspace-restore
  "lW" 'burly-bookmark-windows
  "lF" 'burly-bookmark-frames
  "lB" 'burly-open-bookmark
  "lL" 'burly-open-last-bookmark
  "lV" 'burly-tabs-reset-tab)

(global-set-key (kbd "M-<f12>") 'xy/workspace-restore)
(global-set-key (kbd "C-c C-w s") 'xy/workspace-save)
(global-set-key (kbd "C-c C-w S") 'xy/workspace-save-all)
(global-set-key (kbd "C-c C-w R") 'xy/workspace-restore)
(global-set-key (kbd "C-c C-w w") 'burly-bookmark-windows)
(global-set-key (kbd "C-c C-w f") 'burly-bookmark-frames)
(global-set-key (kbd "C-c C-w b") 'burly-open-bookmark)
(global-set-key (kbd "C-c C-w l") 'burly-open-last-bookmark)
(global-set-key (kbd "C-c C-w t") 'burly-tabs-mode)
(global-set-key (kbd "C-c C-w r") 'burly-tabs-reset-tab)


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
