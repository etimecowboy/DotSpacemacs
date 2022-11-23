;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- chinese-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2022-11-23 Wed 09:14 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; (spacemacs//set-monospaced-font "Cascadia Code" "LXGW WenKai Mono GB" 11 18)
;; for chinese layer `youdao-dcitionary' package
(spacemacs/declare-prefix "oc" "Chinese")

(spacemacs/set-leader-keys
  "oy" 'youdao-dictionary-search-at-point+
  "ocd" 'find-by-pinyin-dired
  "occ" 'chinese-conv-replace
  "ocC" 'chinese-conv)

(global-set-key (kbd "C-x j") 'ace-pinyin-jump-char)