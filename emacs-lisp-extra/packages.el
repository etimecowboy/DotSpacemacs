;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- emacs-lisp-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-02-24 Sat 10:07 by xin on tufg>
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
        ;; cask
        (asoc :location (recipe
                         :fetcher github
                         :repo "troyp/asoc.el"))
        ))

;; (defun emacs-lisp-extra/init-cask ()
;;   (use-package cask))

(defun emacs-lisp-extra/init-asoc ()
  (use-package asoc
    :ensure t))
