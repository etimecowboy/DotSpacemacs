;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- lazycat layer packages file for Spacemacs.
;; Time-stamp: <2023-05-03 Wed 03:00 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq lazycat-packages
      '((watch-other-window :location (recipe
                                       :fetcher github
                                       :repo "manateelazycat/watch-other-window"))
        ))

(defun lazycat/init-watch-other-window ()
  (use-package watch-other-window
    :init
    (spacemacs|define-transient-state watch-other-window
      :title "watch-other-window transient state"
      :doc "
[_C-n_] watch-other-window-up-line [_C-p_] watch-other-window-down-line
[_C-v_] watch-other-window-up      [_M-v_] watch-other-window-down
[_q_]   quit"
      :bindings
      ("q" nil :exit t)
      ("C-n" watch-other-window-up-line)
      ("C-p" watch-other-window-down-line)
      ("C-v" watch-other-window-up)
      ("M-v" watch-other-window-down))
    ))
