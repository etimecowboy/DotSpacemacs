;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- compleseus-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-06-18 Sun 02:25 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(global-set-key (kbd "M-\\") 'completion-at-point)
(global-set-key (kbd "M-*") 'embark-act)
(global-set-key (kbd "M-\"") 'embark-act-noquit)
(global-set-key (kbd "M-L") 'embark-live)
(global-set-key (kbd "M-B") 'embark-become)

(spacemacs/set-leader-keys "Tv" 'vertico-posframe-mode)
(spacemacs/set-leader-keys
  "ip" 'eil-image-insert-path
  "fp" 'eli-image-find-file)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "mp" 'eli-image-org-attach
  "iP" 'eli-image-org-attach)


