;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- lsp-bridge Layer keybindings File for Spacemacs
;; Time-stamp: <2023-02-23 Thu 13:07 by xin on tufg>
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
  "tr" 'xy/toggle-company-lsp-bridge)
