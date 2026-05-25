;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- emacs-extra layer packages file for Spacemacs.
;; Time-stamp: <2026-04-29 Wed 08:09:18 GMT by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;    This layer configures and enhances some official emacs packages.
;;
;;; Code:

(defconst emacs-extra-packages
  '(dired
    diredfl
    dirvish
    info
    goto-chg
    gnu-elpa-keyring-update
    ))


(defun emacs-extra/pre-init-dired ()
  (spacemacs/add-to-hook 'dired-mode-hook
                         '(xy/pretty-dired-buffer)))



(defun emacs-extra/init-diredfl ()
  (use-package diredfl
    :config
    (diredfl-global-mode 1)
    ))


(defun emacs-extra/init-dirvish ()
  ;; ref: `ranger/init-dirvish'
  (use-package dirvish
    :commands (dirvish dirvish-side dirvish-dwim dirvish-quick-access
                       dirvish-fd dirvish-override-dired-mode
                       dirvish-layout-toggle dirvish-layout-switch
                       dirvish-dispatch dirvish-ls-switches-menu
                       dirvish-narrow dirvish-subtree-toggle dirvish-quit)
    ;; :init
    ;; (ranger//set-leader-keys)
    ;; (when (eq ranger-override-dired 'dirvish)
    ;;   (setq dirvish-default-layout nil)
    ;;   (dirvish-override-dired-mode 1))
    :config
    ;; (ranger//apply-override-dired)
    ;; (dirvish-override-dired-mode 1)
    ;; Add icons attribute based on dotspacemacs-default-icons-font.
    ;; We don't override dirvish-attributes or dirvish-mode-line-format
    ;; to respect user customizations via M-x customize.
    (when-let* ((icons-font dotspacemacs-default-icons-font))
      (with-eval-after-load icons-font
        (add-to-list 'dirvish-attributes icons-font)))))


(defun emacs-extra/init-info ()
  (use-package info
    :defer t
    :init
    (add-to-list 'auto-mode-alist
                 '("\\.info\\'" . info-mode))
    :config
    ;; Opens .info files
    (defun xy/open-info ()
      (interactive)
      (let ((file-name (buffer-file-name)))
        (kill-buffer (current-buffer))
        (info file-name)))
    ))


;; NOTE: goto-chg.el was already loaded by a spacemacs. However, it might be a
;; dependency that was not explicitly defined in the `spacemacs-default' layer.
(defun emacs-extra/init-goto-chg ()
  (use-package goto-chg
    :defer t
    ))


(defun emacs-extra/init-gnu-elpa-keyring-update ()
  (use-package gnu-elpa-keyring-update
    :defer t
    ))
