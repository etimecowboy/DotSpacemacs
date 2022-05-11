;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- dired-extra layer packages file for Spacemacs.
;; Time-stamp: <2022-05-11 Wed 10:10 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq dired-extra-packages
      '(
        ;; dired+
        ))

;; (defun dired-extra/init-dired+ ()
;;   (use-package dired+))

;; (define-key dired-mode-map (kbd "\\") 'dired-get-size)

(add-hook 'dired-mode-hook
          #'(lambda ()
              (dired-hide-details-mode t)
              (when window-system
                (text-scale-decrease 1))))

