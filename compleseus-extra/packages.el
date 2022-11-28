;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; packages.el --- compleseus-extra layer packages file for Spacemacs.
;; Time-stamp: <2022-11-24 Thu 17:29 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq compleseus-extra-packages
      '(
        consult-dir
        (eli-image :location local)
        ;; consult-project-extra ;; not as good as consult-projectile
        consult-projectile
        ;; consult-flycheck
        ;; consult-flyspell
        consult-company
        ))

(defun compleseus-extra/init-consult-dir ()
  (use-package consult-dir
    :ensure t
    :bind (("C-x C-d" . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file))))

(defun compleseus-extra/init-eli-image ()
  (use-package eli-image
    :commands (eli-select-images)
    ))

;; (defun compleseus-extra/init-consult-project-extra ()
;;   (use-package consult-project-extra))

;; TODO: add consult/selectrum keymap (M-s) bindings
(defun compleseus-extra/init-consult-projectile ()
  (use-package consult-projectile
    :bind (
           ("M-s p" . consult-projectile)
           )))

;; TODO: add consult/selectrum keymap (M-s) bindings
(defun compleseus-extra/init-consult-company ()
  (use-package consult-company
    :bind (
           ("M-s c" . consult-company)
           )
    ))
