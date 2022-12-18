;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- Hyperbole layer packages file for Spacemacs.
;; Time-stamp: <2022-12-13 Tue 18:56 by xin on tufg>
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
    :bind*
    ("<M-return>" . hkey-either)
    ;; :bind
    ;; ("M-o" . nil) ;;conflict with embark
    ))
