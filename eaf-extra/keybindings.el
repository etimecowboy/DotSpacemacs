;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- eaf-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-01-05 Thu 07:08 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/set-leader-keys "te" 'xy/toggle-eaf-browser)
(spacemacs/set-leader-keys "aabt" 'xy/toggle-eaf-browser)
(spacemacs/set-leader-keys "aar" 'eaf-open-rss-reader)
(spacemacs/set-leader-keys "aag" 'eaf-open-git)
(spacemacs/set-leader-keys "aap" 'eaf-open-pdf-from-history)
(spacemacs/set-leader-keys "aai" 'eaf-open-ipython)
