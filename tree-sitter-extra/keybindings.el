;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- tree-sitter-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-03-02 Thu 02:16 by xin on tufg>
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
(spacemacs/declare-prefix "tT" "tree-sitter")
(spacemacs/set-leader-keys
  "ott" 'ts-fold-open-recursively
  "oto" 'ts-fold-open
  "otO" 'ts-fold-open-all
  "otc" 'ts-fold-close
  "otC" 'ts-fold-close-all
  "tTt" 'tree-sitter-mode
  "tIi" 'tree-sitter-indent-mode
  "tTf" 'ts-fold-toggle)
