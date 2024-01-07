;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- dired-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-01-05 Fri 02:58 by xin on tufg>
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
  '(dired
    ))

(defun dired-extra/pre-init-dired ()
  (spacemacs/add-to-hook 'dired-mode-hook
                         '(xy/pretty-dired-buffer)))
