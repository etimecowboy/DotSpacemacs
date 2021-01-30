;;; packages.el --- Chinese-extra Layer packages File for Spacemacs
;; Time-stamp: <2021-01-27 Wed 09:40 by xin on legion>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(setq chinese-extra-packages
      '(
        dictionary
        ))

(defun chinese-extra/init-dictionary ()
  (use-package dictionary
    :defer t
    :init
    (progn
      ;; (dictionary-tooltip-mode 1)
      ;; (global-dictionary-tooltip-mode 1)
      )))
