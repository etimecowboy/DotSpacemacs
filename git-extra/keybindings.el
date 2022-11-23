;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- git-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2022-11-23 Wed 07:00 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; TODO move to the layer
;; (global-git-commit-mode t)
;; (put 'helm-make-build-dir 'safe-local-variable 'stringp)
(spacemacs/set-leader-keys "gT" 'git-timemachine-toggle)
