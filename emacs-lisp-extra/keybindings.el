;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- emacs-lisp-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2024-03-21 Thu 03:58 by xin on tufg>
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
  "hb" 'esup)

(global-set-key (kbd "C-x C-k i") 'xy/insert-kbd-macro)
;; (global-set-key (kbd "C-x C-k i") 'insert-kbd-macro)
