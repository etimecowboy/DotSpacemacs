;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; keybindings.el --- engine-extra Layer keybindings File for Spacemacs
;; Time-stamp: <2023-06-30 Fri 08:13 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; (spacemacs/declare-prefix "aw" "web search")

(spacemacs/set-leader-keys
  "aw/" nil ;; I use neither helm nor ivy.
  "awa" 'engine/search-amazon
  "awb" 'engine/search-bing
  "awd" 'engine/search-docker-hub
  "awD" 'engine/search-duck-duck-go
  "awh" 'engine/search-github
  "awg" 'engine/search-google
  "awi" 'engine/search-google-images
  "awm" 'engine/search-google-maps
  "awt" 'engine/search-twitter
  "awG" 'engine/search-project-gutenberg
  "awy" 'engine/search-youtube
  "aws" 'engine/search-stack-overflow
  "aww" 'engine/search-wikipedia
  "awp" 'engine/search-pip
  "awP" 'engine/search-python-doc
  "awc" 'engine/search-c++-api-reference
  "awu" 'engine/search-ubuntu-packages
  "awe" 'engine/search-melpa
  "awC" 'engine/search-ctan
  )
