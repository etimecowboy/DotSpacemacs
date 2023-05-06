;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- conda-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-05-06 Sat 04:07 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst conda-extra-packages
  '(conda))

(defun conda-extra/pre-init-conda ()
  (spacemacs|use-package-add-hook conda
    :post-config
    ;; interactive shell support
    (conda-env-initialize-interactive-shells)
    ;; eshell support
    (conda-env-initialize-eshell)
    ;; auto-activation
    (conda-env-autoactivate-mode t)
    ;; automatically activate a conda environment on the opening of a file
    (add-hook 'find-file-hook
              (lambda ()
                (when (bound-and-true-p conda-project-env-path)
                  (conda-env-activate-for-buffer))))
    ))
