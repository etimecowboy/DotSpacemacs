;;; config.el --- ispell-extra configuration File for Spacemacs
;; Time-stamp: <2022-11-23 Wed 09:06 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:

;; ispell is included in layer spell-checking
(with-eval-after-load "ispell"
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
