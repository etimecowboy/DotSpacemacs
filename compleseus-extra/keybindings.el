;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- compleseus-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-06-30 Fri 01:21 by xin on tufg>
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
(global-set-key (kbd "C-S-g") 'posframe-delete-all)

(spacemacs/set-leader-keys "Tv" 'vertico-posframe-mode)
(spacemacs/set-leader-keys
  "ip" 'eil-image-insert-path
  "fp" 'eli-image-find-file
  "ty" 'spacemacs/load-yasnippet
  "tY" 'spacemacsforce-yasnippet-off)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "mi" 'eli-image-org-attach
  "mp" 'org-preview-image-link-posframe
  )
