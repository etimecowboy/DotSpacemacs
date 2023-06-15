;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- emacs-demo layer packages file for Spacemacs.
;; Time-stamp: <2023-06-15 Thu 04:11 by xin on tufg>
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
        (screenshot :location
                    (recipe :fetcher github
                            :repo "tecosaur/screenshot"))
        org-tree-slide ;; required by demo-it
        fancy-narrow ;; required by demo-it
        demo-it
        ))

;; load command-log-mode
(defun emacs-demo/init-command-log-mode ()
  (use-package command-log-mode))

;; load gif-screencast
(defun emacs-demo/init-gif-screencast ()
  (use-package gif-screencast
    :bind
    (:map gif-screencast-mode-map
          ("<f7>" . gif-screencast-start-or-stop)
          ("<f8>" . gif-screencast-toggle-pause)
          ("<f9>" . gif-screencast-stop))))

;; load keycast
(defun emacs-demo/init-keycast ()
  (use-package keycast
    :config
    (setq keycast-tab-bar-location  'replace
          keycast-tab-bar-format "%k%2s%c%R")))

;; load screenshot
(defun emacs-demo/init-screenshot ()
  (use-package screenshot
    ;; :config
    ;; (setq screenshot-border-width 0
    ;;       screenshot-line-numbers-p t
    ;;       screenshot-relative-line-numbers-p t)
    ))

;; load org-tree-slide
(defun emacs-demo/init-org-tree-slide ()
  (use-package org-tree-slide
    :bind
    (:map org-tree-slide-mode-map
          ("<f7>"  . org-tree-slide-move-previous-tree)
          ("<f8>" . org-tree-slide-move-next-tree)
          ("<f9>" . org-tree-slide-content))
    :custom
    (setq org-tree-slide-skip-outline-level 4)
    (org-tree-slide-narrowing-control-profile)
    (setq org-tree-slide-skip-done nil)))

;; load fancy-narrow
(defun emacs-demo/init-fancy-narrow ()
  (use-package fancy-narrow))

;; load demo-it
(defun emacs-demo/init-demo-it ()
  (use-package demo-it
    :after org
    :config
    (setq demo-it--shell-or-eshell :shell
          demo-it--text-scale 4)
    ))
