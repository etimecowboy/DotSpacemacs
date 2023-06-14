;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- lsp-bridge Layer keybindings File for Spacemacs
;; Time-stamp: <2023-06-11 Sun 12:29 by xin on tufg>
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
  "te" 'lsp-bridge-toggle-sdcv-helper
  "tq" 'lsp-bridge-mode
  ;; "tr" 'xy/toggle-company-lsp-bridge
  )
