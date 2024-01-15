;;; packages.el --- spell-checking-extra layer packages File for Spacemacs
;; Time-stamp: <2024-01-08 Mon 13:14 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;
;;; Code:

(defconst spell-checking-extra-packages '(ispell))

(defun spell-checking-extra/pre-init-ispell ()
  (spacemacs|use-package-add-hook ispell
    :post-config
    ;; aspell works great, but hunspell is more accurate.
    ;; ispell-program-name "aspell"
    ;; ispell-dictionary "american"
    (setq ispell-program-name "hunspell")
    ;; ispell-set-spellchecker-params has to be called
    ;; before ispell-hunspell-add-multi-dic will work
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,en_GB")
    (setq ispell-dictionary "en_US,en_GB")
    (ispell-change-dictionary "en_US" t)
    ))
