;;; config.el --- tree-sitter-extra configuration File for Spacemacs
;; Time-stamp: <2022-12-29 Thu 03:31 by xin on tufg>
;; Author: etimecowboy <etimecowboy@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;

;;; Code:
(with-eval-after-load "tree-sitter"
  (spacemacs|diminish tree-sitter-mode " Ⓣ" " T"))

(with-eval-after-load "tree-sitter-indent"
  (spacemacs|diminish tree-sitter-mode " Ⓘ" " I"))

(with-eval-after-load "ts-fold"
  (spacemacs|diminish ts-fold-mode " Ⓕ" " F"))
