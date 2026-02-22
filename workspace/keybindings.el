;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- workspace Layer keybindings File
;; Time-stamp: <2025-12-30 Tue 09:11:30 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/declare-prefix "W" "workspace")
;; (spacemacs/declare-prefix "Ww" "window")

(spacemacs/set-leader-keys
  "Tw" 'burly-tabs-mode
  "Ws" 'xy/workspace-save
  "WS" 'xy/workspace-save-all
  "WR" 'xy/workspace-restore
  "WW" 'burly-bookmark-windows
  "WF" 'burly-bookmark-frames
  "WB" 'burly-open-bookmark
  "WL" 'burly-open-last-bookmark
  "WV" 'burly-tabs-reset-tab
  "W C-o" 'burly-open-url
  "W C-w" 'burly-kill-windows-url
  "W C-f" 'burly-kill-frames-url
  "W C-b" 'burly-kill-buffer-url

  ;; Moved from UI layer
  "Wt"  'tab-new
  "WT"  'tab-new-to
  "W C-f" 'find-file-other-tab
  "W C-r" 'find-file-read-only-other-tab
  ;; "Wo"  'tab-next
  "Wn"  'tab-next
  ;; "WO"  'tab-previous
  "Wp"  'tab-previous
  "W RET" 'tab-switch
  "Wu"  'tab-undo
  "Wm"  'tab-move
  "WM"  'tab-move-to
  "W0"  'tab-close
  "W1"  'tab-close-other
  "Wr"  'tab-rename
  "Ww"  'tab-window-detach
  "Wd"  'tab-detach
  "Wc"  'tab-duplicate
  "WG"  'tab-group
  "WD"  'dired-other-tab
  "Wx"  'other-tab-prefix
  "WP"  'project-other-tab-command
  "Wc" 'tab-line-new-tab
  "Wq" 'bury-buffer ;; close the current tab-line buffer
  "Wf" 'tab-line-switch-to-next-tab
  "Wb" 'tab-line-switch-to-prev-tab
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
;;   "W"  'spacemacs/workspaces-transient-state/body
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
