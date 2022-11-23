;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- compleseus-extra layer packages file for Spacemacs.
;; Time-stamp: <2022-11-23 Wed 03:34 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq compleseus-extra-packages
      '(
        consult-dir
        ))

(defun compleseus-extra/init-consult-dir ()
  (use-package consult-dir
    :ensure t
    :bind (("C-x C-d" . consult-dir)
           :map selectrum-minibuffer-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file))))
