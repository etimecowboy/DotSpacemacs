;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- Hyperbole layer packages file for Spacemacs.
;; Time-stamp: <2023-06-15 Thu 15:37 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:
(setq hyperbole-packages
      '(
        hyperbole
        ))

(defun hyperbole/init-hyperbole ()
  (use-package hyperbole
    :config
    (spacemacs|diminish hyperbole-mode " Ⓗ" " H")
    (setq hyperbole-mode-lighter " Ⓗ")
    ;; :bind*
    ;; ("C-:" . hkey-either)
    ;; :bind
    ;; ("M-o" . nil) ;;conflict with embark
    ))
