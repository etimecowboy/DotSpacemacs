;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- subed layer packages file for Spacemacs.
;; Time-stamp: <2022-11-23 Wed 07:04 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq subed-packages
      '(
        subed
        ))

(defun subed/init-subed ()
  (use-package subed
    :init
    ;; Disable automatic movement of point by default
    ;; (add-hook 'subed-mode-hook 'subed-disable-sync-point-to-player)
    ;; Remember cursor position between sessions
    (add-hook 'subed-mode-hook 'save-place-local-mode)))
