;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- browsers Layer keybindings File for Spacemacs
;; Time-stamp: <2023-10-22 Sun 03:26 by xin on tufg>
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
  "abB"  'xy/set-eaf-browser-as-default-browser
  "abb"  'xy/set-brave-as-default-browser
  "abc"  'xy/set-google-chrome-as-default-browser
  "abe"  'xy/set-eww-as-default-browser
  "abw"  'xy/set-w3m-as-default-browser
  "abo"  'browse-url-at-point
  "abC"  'browse-url-chrome
  "abW"  'w3m-browse-url
  "abB"  'w3m-bookmark-view
  "abE"  'eaf-open-bookmark
  "jj"   'avy-goto-char
  "jJ"   'avy-goto-char-2
  "jo"   'link-hint-open-link
  "jO"   'link-hint-copy-link
  )
