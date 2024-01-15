;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- all-the-icons-extra layer packages file for Spacemacs.
;; Time-stamp: <2024-01-08 Mon 05:54 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst all-the-icons-extra-packages
  '(
    ;; all-the-icons ;; init in `spacemacs-visual' layer 
    all-the-icons-ibuffer
    all-the-icons-dired
    ;; helm-icons ;; helm is removed
    ))

(defun all-the-icons-extra/init-all-the-icons-ibuffer ()
  (use-package all-the-icons-ibuffer
    :hook
    (ibuffer-mode . all-the-icons-ibuffer-mode)
    ))

(defun all-the-icons-extra/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    ;; :hook
    ;; (dired-mode . all-the-icons-dired-mode)
    :init
    (add-hook 'dired-mode-hook
              (lambda ()
                (if window-system
                    (all-the-icons-dired-mode -1)
                  (all-the-icons-dired-mode t))))
    :config
    ;; improve font rendering performance
    (setq inhibit-compacting-font-caches t)
    ))

;; (defun all-the-icons-extra/init-helm-icons ()
;;   (use-package helm-icons
;;     :init
;;     (helm-icons-enable)
;;     :custom
;;     (setq helm-icons-provider 'all-the-icons)))
