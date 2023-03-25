;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- chinese-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-03-23 Thu 03:20 by xin on tufg>
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
  "ocd" 'find-by-pinyin-dired
  "occ" 'chinese-conv-replace
  "ocC" 'chinese-conv
  "ocs" 'pangu-spacing-space-current-buffer
  "ocy" 'youdao-dictionary-search-at-point+
  "ocj" 'ace-pinyin-jump-char
  "ocw" 'ace-pinyin-jump-word)

(global-set-key (kbd "C-x j") 'ace-pinyin-jump-char)
