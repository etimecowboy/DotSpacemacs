;;; funcs.el --- popweb Layer functions File for Spacemacs
;; Time-stamp: <2023-03-23 Thu 03:24 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

(defun xy/youdao-at-point ()
  "Look up word/region using Youdao web service."
  (interactive)

  (if window-system
      (popweb-dict-youdao-pointer)
    (youdao-dictionary-search-at-point+)))
