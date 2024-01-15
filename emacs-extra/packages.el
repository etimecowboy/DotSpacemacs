;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- emacs-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-01-07 Sun 15:00 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;    This layer configures and enhances some official emacs packages.
;;
;;; Code:

(defconst emacs-extra-packages
  '(dired
    info))

(defun emacs-extra/pre-init-dired ()
  (spacemacs/add-to-hook 'dired-mode-hook
                         '(xy/pretty-dired-buffer)))

(defun emacs-extra/init-info ()
  (use-package info
    :defer t
    :init
    (add-to-list 'auto-mode-alist
                 '("\\.info\\'" . info-mode))
    :config
    ;; Opens .info files
    (defun xy/open-info ()
      (interactive)
      (let ((file-name (buffer-file-name)))
        (kill-buffer (current-buffer))
        (info file-name)))
    ))
