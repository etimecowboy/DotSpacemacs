;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- engine-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2022-11-23 Wed 06:57 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/set-leader-keys
  "awg" 'engine/search-google
  "awi" 'engine/search-google-images
  "awG" 'engine/search-github
  "awb" 'engine/search-bing
  "aww" 'engine/search-wikipedia
  "awe" 'engine/search-melpa
  "aws" 'engine/search-stack-overflow
  "awy" 'engine/search-youtube)
