;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- lsp-bridge Layer keybindings File for Spacemacs
;; Time-stamp: <2023-07-29 Sat 13:24 by xin on tufg>
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
  "tq" 'lsp-bridge-mode
  "L"  'spacemacs/lsp-bridge-transient-state/body
  )
