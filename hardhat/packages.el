;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- hardhat layer packages file for Spacemacs.
;; Time-stamp: <2022-11-23 Wed 07:09 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:
(setq hardhat-packages
      '(
        hardhat
        ))

(defun hardhat/init-hardhat ()
  (use-package hardhat
               :ensure t))
