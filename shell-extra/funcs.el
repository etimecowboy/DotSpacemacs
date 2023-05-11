;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- Shell-extra Layer functions File for Spacemacs
;; Time-stamp: <2023-05-11 Thu 07:58 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; Add some more C- M- key sequences that used by terminal apps.
;; zellij lock-mode  prefix
(defun vterm-send-ctrl-g ()
  "Seng `C-g' to the libvterm."
  (interactive)
  (vterm-send-key "g" nil nil t))

;; tmux default prefix
(defun vterm-send-ctrl-b ()
  "Seng `C-b' to the libvterm."
  (interactive)
  (vterm-send-key "b" nil nil t))

;; (defun vterm-send-meta-return ()
;;   "Seng `M-<return>' to the libvterm."
;;   (interactive)
;;   (vterm-send-key "<return>" nil t))
