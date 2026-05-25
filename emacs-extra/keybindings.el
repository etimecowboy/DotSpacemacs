;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- dired-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2026-04-29 Wed 06:32:26 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(define-key dired-mode-map (kbd "\\") 'xy/dired-get-size)

(spacemacs/set-leader-keys
  "jc" 'goto-last-change ;; same as default spacemacs key.
  "jC" 'goto-last-change-reverse)

;; Define more keybindings for Copy&Paste from the system clipboard,
;; which work in both CLI and GUI emacs frames.
(global-set-key (kbd "C-S-w") 'spacemacs/xclipboard-copy)
(global-set-key (kbd "C-S-v") 'spacemacs/xclipboard-paste)
