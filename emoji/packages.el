;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- emoji layer packages file for Spacemacs.
;; Time-stamp: <2024-03-05 Tue 04:28 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;    This layer overrides the official emoji layer.
;;    Native `emoji.el' package is preferred.
;;
;;; Code:

(defconst emoji-packages
  '(
    emojify))

(defun emoji/init-emojify ()
  (use-package emojify
    :defer t
    :init
    (setq emojify-emojis-dir (concat spacemacs-cache-directory "emojify/"))
    :config
    (setq emojify-display-style 'unicode)
    (setq emojify-emoji-styles '(unicode))
    ;; (global-emojify-mode 1)
    :bind
    (("C-c ." . emojify-insert-emoji))
    ;; (bind-key* (kbd "C-c .") #'emojify-insert-emoji) ; override binding in any mode
    ))
