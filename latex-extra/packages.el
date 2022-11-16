;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- latex-extra layer packages file for Spacemacs.
;; Time-stamp: <2022-10-28 Fri 09:48 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq latex-extra-packages
      '(cdlatex
        auctex-latexmk
        ))

(defun latex-extra/init-cdlatex ()
  (spacemacs|use-package-add-hook org :post-config (require 'cdlatex))
  (spacemacs|use-package-add-hook tex :post-config (require 'cdlatex))
  (use-package cdlatex
    :defer t
    :hook
    (org-mode . turn-on-org-cdlatex)
    (LaTeX-mode . turn-on-org-cdlatex)))

(defun latex-extra/init-auctex-latexmk ()
  (use-package auctex-latexmk
    :defer t
    :hook
    (LaTeX-mode . auctex-latexmk-setup)
    :int
    (setq auctex-latexmk-inherit-TeX-PDF-mode t)))
