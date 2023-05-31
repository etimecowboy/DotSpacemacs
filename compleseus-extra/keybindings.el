;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- compleseus-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-05-29 Mon 06:26 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(global-set-key (kbd "M-\\") 'completion-at-point)
(global-set-key (kbd "M-p") 'embark-act)
(global-set-key (kbd "M-P") 'embark-act-noquit)
(global-set-key (kbd "M-L") 'embark-live)
(global-set-key (kbd "M-B") 'embark-become)

;; (spacemacs/set-leader-keys "Tv" 'vertico-posframe-mode)

