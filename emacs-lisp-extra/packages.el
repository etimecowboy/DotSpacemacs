;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- emacs-lisp-extra layer packages file for Spacemacs.
;; Time-stamp: <2022-09-17 Sat 02:31 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq emacs-lisp-extra-packages
      '(
        cask
        ))

(defun emacs-lisp-extra/init-cask ()
  (use-package cask))


