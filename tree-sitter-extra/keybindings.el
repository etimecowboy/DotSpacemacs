;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- tree-sitter-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2022-12-29 Thu 03:38 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/declare-prefix "ot" "tree-sitter")
(spacemacs/set-leader-keys
  "ott" 'ts-fold-open-recursively
  "oto" 'ts-fold-open
  "otO" 'ts-fold-open-all
  "otc" 'ts-fold-close
  "otC" 'ts-fold-close-all)
