;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- compleseus-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-04-20 Thu 03:28 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs|use-package-add-hook embark
  :post-config
  ;; (global-set-key (kbd "M-o") 'embark-act-quit)
  (global-set-key (kbd "M-O") 'embark-act-noquit)
  (global-set-key (kbd "M-L") 'embark-live)
  (global-set-key (kbd "M-B") 'embark-become)
  )

(global-set-key (kbd "M-\\") 'completion-at-point)

(spacemacs/set-leader-keys "tV" 'vertico-posframe-mode)
