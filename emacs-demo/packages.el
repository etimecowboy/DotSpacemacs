;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- emacs-demo layer packages file for Spacemacs.
;; Time-stamp: <2022-09-25 Sun 01:32 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq emacs-demo-packages
      '(
        command-log-mode
        gif-screencast
        keycast
        ))

;; load command-log-mode
(defun emacs-demo/init-command-log-mode ()
  (use-package command-log-mode
    :init
    (spacemacs/declare-prefix "aD" "demo")
    (spacemacs/set-leader-keys "aDc" 'command-log-mode)
    (spacemacs/set-leader-keys "aDl" 'clm/toggle-command-log-buffer)
    (spacemacs/set-leader-keys "aDC" 'clm/command-log-clear)
    (spacemacs/set-leader-keys "aDS" 'clm/command-log-save)))

;; load gif-screencast
(defun emacs-demo/init-gif-screencast ()
  (use-package gif-screencast
    :init
    (spacemacs/declare-prefix "aD" "demo")
    (spacemacs/set-leader-keys "aDg" 'gif-screencast)
    :bind
    (:map gif-screencast-mode-map
          ("<f8>" . gif-screencast-toggle-pause)
          ("<f9>" . gif-screencast-stop))))

;; load keycast
(defun emacs-demo/init-keycast ()
  (use-package keycast
    :init
    (spacemacs/declare-prefix "aD" "demo")
    (spacemacs/set-leader-keys "aDt" 'keycast-tab-bar-mode)
    :config
    (setq keycast-tab-bar-location  'replace
          keycast-tab-bar-format "%k%2s%c%R")))
