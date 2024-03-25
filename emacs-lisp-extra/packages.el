;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- emacs-lisp-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-03-21 Thu 03:57 by xin on tufg>
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
      '((asoc :location (recipe :fetcher github :repo "troyp/asoc.el"))
        esup ;; record bootup time
        ;; cask
        ))

(defun emacs-lisp-extra/init-asoc ()
  (use-package asoc
    :ensure t))

(defun emacs-lisp-extra/init-esup ()
  (use-package esup
    :commands esup
    ;; To use MELPA Stable use ":pin melpa-stable",
    ;; :pin melpa
    :config
    (setq esup-user-init-file "~/src/spacemacs/init.el"
          esup-depth 3
          esup-insignificant-time 0.001)
    ))


;; (defun emacs-lisp-extra/init-cask ()
;;   (use-package cask))
