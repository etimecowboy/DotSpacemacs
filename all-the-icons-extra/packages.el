;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- all-the-icons-extra layer packages file for Spacemacs.
;; Time-stamp: <2023-03-16 Thu 08:06 by xin on tufg>
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
    ;; all-the-icons
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

;; (defun all-the-icons/init-all-the-icons ()
;;   (use-package all-the-icons
;;     :defer t))

;; (defun all-the-icons/init-helm-icons ()
;;   (use-package helm-icons
;;     :init
;;     (helm-icons-enable)
;;     :custom
;;     (setq helm-icons-provider 'all-the-icons)))

;; ;; package: all-the-icons-dired
;; (use-package all-the-icons-dired
;;   :init
;;   (add-hook 'dired-mode-hook
;;             (lambda ()
;;               (if window-system
;;                   (all-the-icons-dired-mode -1)
;;                 (all-the-icons-dired-mode t))))
;;   ;; improve font rendering performance
;;   (setq inhibit-compacting-font-caches t))
