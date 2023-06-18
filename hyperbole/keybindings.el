;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- hyperbole Layer keybindings File for Spacemacs
;; Time-stamp: <2023-06-15 Thu 15:37 by xin on tufg>
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
  "tH" 'hyperbole-mode)

(global-set-key (kbd "C-:") 'hkey-either)
