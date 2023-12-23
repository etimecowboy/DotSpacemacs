;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- spacemacs-visual-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-12-23 Sat 14:23 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst spacemacs-editing-visual-extra-packages
  '(writeroom-mode))

(defun spacemacs-editing-visual-extra/post-init-writeroom-mode ()
  (setq writeroom-extra-line-spacing 0.5
        writeroom-global-effects
        '(writeroom-set-fullscreen
          writeroom-set-alpha
          writeroom-set-menu-bar-lines
          writeroom-set-tool-bar-lines
          writeroom-set-vertical-scroll-bars
          writeroom-set-bottom-divider-width
          writeroom-set-internal-border-width)
        ;; writeroom-header-line t
        writeroom-bottom-divider-width 2
        writeroom-restore-window-config t)

  (add-hook 'writeroom-mode #'virtual-line-mode))
