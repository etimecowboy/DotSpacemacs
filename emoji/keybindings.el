;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- emoji Layer keybindings File for Spacemacs
;; Time-stamp: <2024-03-05 Tue 02:07 by xin on tufg>
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
  "ie" 'emoji-insert        ;; "C-x 8 e e"
  ;; "ii" 'emoji-insert        ;; "C-x 8 e i"
  "ii" 'emojify-insert-emoji
  "ir" 'emoji-recent        ;; "C-x 8 e r"
  "id" 'emoji-describe      ;; "C-x 8 e d"
  "iL" 'emoji-list          ;; "C-x 8 e l"
  "iS" 'emoji-search        ;; "C-x 8 e s"
  "i+" 'emoji-zoom-increase ;; "C-x 8 e +"
  "i-" 'emoji-zoom-decrease ;; "C-x 8 e -"
  "i0" 'emoji-zoom-reset    ;; "C-x 8 e 0"
  "te" 'global-emojify-mode
)
