;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- dired-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-05-15 Mon 16:41 by xin on tufg>
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
