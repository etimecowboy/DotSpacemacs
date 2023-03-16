;;; config.el --- ispell-extra configuration File for Spacemacs
;; Time-stamp: <2023-03-16 Thu 08:35 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;; ispell is included in layer spell-checking

;;; Code:

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
  (ispell-change-dictionary "en_US" t))
