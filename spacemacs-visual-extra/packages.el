;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- spacemacs-visual-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-05-06 Sat 06:39 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst spacemacs-visual-extra-packages
  '(
    ;; all-the-icons
    all-the-icons-ibuffer
    all-the-icons-dired
    ;; helm-icons ;; helm is removed
    ))

(defun spacemacs-visual-extra/init-all-the-icons-ibuffer ()
  (use-package all-the-icons-ibuffer
    :hook
    (ibuffer-mode . all-the-icons-ibuffer-mode)
    ))

(defun spacemacs-visual-extra/init-all-the-icons-dired ()
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

;; (defun spacemacs-visual-extra/init-helm-icons ()
;;   (use-package helm-icons
;;     :init
;;     (helm-icons-enable)
;;     :custom
;;     (setq helm-icons-provider 'all-the-icons)))
