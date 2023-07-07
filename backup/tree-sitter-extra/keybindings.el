;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- tree-sitter-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-03-16 Thu 07:23 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; (spacemacs/declare-prefix "ot" "tree-sitter")
;; (spacemacs/declare-prefix "tT" "tree-sitter")
;; (spacemacs/set-leader-keys
;;   "ott" 'ts-fold-open-recursively
;;   "oto" 'ts-fold-open
;;   "otO" 'ts-fold-open-all
;;   "otc" 'ts-fold-close
;;   "otC" 'ts-fold-close-all
;;   "tTt" 'tree-sitter-mode
;;   "tIi" 'tree-sitter-indent-mode
;;   "tTf" 'ts-fold-toggle)

;; (spacemacs/declare-prefix "of" "ts-fold")
;; (spacemacs/set-leader-keys
;;   "oft" 'ts-fold-toggle
;;   "ofo" 'ts-fold-open-recursively ;; "oto" 'ts-fold-open
;;   "ofO" 'ts-fold-open-all
;;   "ofc" 'ts-fold-close
;;   "ofC" 'ts-fold-close-all)
(spacemacs/declare-prefix "tT" "tree-sitter")
(spacemacs/set-leader-keys
  "tTt" 'tree-sitter-mode
  "tTh" 'tree-sitter-hl-mode
  ;; "tTi" 'tree-sitter-indent-mode
  ;; "tTf" 'ts-fold-toggle
  ;; "tTO" 'ts-fold-open-all
  ;;"tTC" 'ts-fold-close-all
  )
