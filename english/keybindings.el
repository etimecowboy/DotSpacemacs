;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- english Layer keybindings File for Spacemacs
;; Time-stamp: <2023-03-10 Fri 07:58 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/declare-prefix "oe" "English")

(spacemacs/set-leader-keys
  "oy" 'youdao-dictionary-search-at-point+)
