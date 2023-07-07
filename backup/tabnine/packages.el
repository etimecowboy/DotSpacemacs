;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- TabNine layer packages file for Spacemacs.
;; Time-stamp: <2023-02-22 Wed 14:46 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst tabnine-packages
  '(
    company-tabnine))

(defun tabnine/init-company-tabnine ()
  (use-package company-tabnine
    :defer t
    :after company
    :init
    (add-to-list 'company-backends #'company-tabnine)))
