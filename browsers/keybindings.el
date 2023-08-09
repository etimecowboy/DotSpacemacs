;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- browsers Layer keybindings File for Spacemacs
;; Time-stamp: <2023-08-08 Tue 07:23 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(spacemacs/declare-prefix "ab" "browsers")

(spacemacs/set-leader-keys
  "abb"  'xy/set-eaf-browser-as-default-browser
  "abc"  'xy/set-google-chrome-as-default-browser
  "abe"  'xy/set-eww-as-default-browser
  "abw"  'xy/set-w3m-as-default-browser)
