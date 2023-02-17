;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- Everywhere layer packages file for Spacemacs.
;; Time-stamp: <2023-02-17 Fri 00:58 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq everywhere-packages
      '(
        emacs-everywhere
        ))

(defun everywhere/init-emacs-everywhere ()
  (use-package emacs-everywhere
    ))
