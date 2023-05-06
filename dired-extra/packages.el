;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- dired-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-05-06 Sat 04:21 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst dired-extra-packages
  '(dired))

(defun dired-extra/pre-init-dired ()
  (spacemacs/add-to-hook 'dired-mode-hook
                         '(xy/pretty-dired-buffer)))
