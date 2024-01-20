;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- dired-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2024-01-19 Fri 01:53 by xin on tufg>
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
  
