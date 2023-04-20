;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- chinese-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-04-20 Thu 03:29 by xin on tufg>
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
  "ocP" 'pangu-spacing-space-current-buffer
  "ocy" 'youdao-dictionary-search-at-point+
  "ocj" 'ace-pinyin-jump-char
  "ocw" 'ace-pinyin-jump-word
  "tP"  'pangu-spacing-mode)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "P" 'pangu-spacing-org-mode-at-special-region)

(global-set-key (kbd "C-x j") 'ace-pinyin-jump-char)
(global-set-key (kbd "<f7>") 'toggle-input-method)

